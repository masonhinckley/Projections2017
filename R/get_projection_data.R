### Get data for desired season projections

get_projection_data <- function(projection.year){

  library(XML)
  library(dplyr)

  first.year <- projection.year - 3
  last.year <- projection.year - 1

  raw.data <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,3,7,8,14,20,21,19,24,26,25&season=", last.year, "&month=0&season1=", first.year, "&ind=1&team=&rost=&age=&filter=&players=&page=1_5000"), stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00[,2:14]
  for (i in c(1,4:13)){
    class(raw.data[,i]) <- "numeric"
  }

  pf.2015 <- readHTMLTable("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2015", stringsAsFactors = FALSE)$GutsBoard1_dg1_ctl00
  pf.2014 <- readHTMLTable("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2014", stringsAsFactors = FALSE)$GutsBoard1_dg1_ctl00
  for (i in c(1,3:14)){
    class(pf.2015[,i]) <- "numeric"
    class(pf.2014[,i]) <- "numeric"
  }
  pf.2016 <- pf.2015
  pf.2016$Season <- 2016

  pf.2016[,3:14] <- pf.2016[,3:14]/100
  pf.2015[,3:14] <- pf.2015[,3:14]/100
  pf.2014[,3:14] <- pf.2014[,3:14]/100

  fgpf <- rbind(pf.2016, pf.2014, pf.2015)

  raw.data <- left_join(raw.data, fgpf, by = c("Team" = "Team", "Season" = "Season"))

  names(raw.data)[10:13] <- c("BB", "SO", "FB", "GB")

  raw.data <- raw.data %>% mutate(BB.y = ifelse(Team == "- - -", 1, BB.y)) %>%
    mutate(SO.y = ifelse(Team == "- - -", 1, SO.y)) %>% mutate(FB.y = ifelse(Team == "- - -", 1, FB.y)) %>%
    mutate(GB.y = ifelse(Team == "- - -", 1, GB.y))

  raw.data <- raw.data %>% mutate(BB = BB/BB.y) %>% mutate(SO = SO/SO.y) %>% mutate(FB = FB/FB.y) %>%
    mutate(GB = GB/GB.y)

  raw.data <- raw.data[,c(1:13)]

  raw.data <- raw.data %>% mutate(HBP. = HBP/TBF) %>% mutate(K. = SO/TBF) %>% mutate(BB. = BB/TBF) %>%
    mutate(FB. = FB/(TBF - BB - IBB - SO - HBP)) %>% mutate(GB. = GB/(TBF - BB - IBB - SO - HBP))

  raw.data[is.na(raw.data)] <- 0

  return(raw.data)

}
