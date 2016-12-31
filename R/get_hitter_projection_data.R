### Get hitter projection data

get_hitter_projection_data <- function(projection.year){

  library(XML)
  library(dplyr)

  first.year <- projection.year - 3
  last.year <- projection.year - 1

  raw.data <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,3,6,5,8,9,10,11,14,15,17,18,53,54,111&season=", last.year, "&month=0&season1=", first.year, "&ind=1&team=0&rost=0&age=0&filter=&players=0&page=1_5000"), stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00[,2:18]

  for (i in 4:length(names(raw.data))){
    class(raw.data[,i]) <- "numeric"
  }

  raw.data <- raw.data %>% mutate(HBP. = HBP/PA) %>% mutate(BB. = BB/PA) %>% mutate(IBB. = IBB/PA) %>%
    mutate(SF. = SF/PA) %>% mutate(HR. = HR/PA) %>% mutate(b3. = `3B`/PA) %>% mutate(b2. = `2B`/PA) %>%
    mutate(b1. = `1B`/PA) %>% mutate(bat. = Bat/PA) %>% mutate(fld. = Fld/PA) %>% mutate(bsr. = BsR/PA)

  raw.data[is.na(raw.data)] <- 0

  return(raw.data)

}
