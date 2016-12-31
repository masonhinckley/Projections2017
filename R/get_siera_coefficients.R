### SIERA coefficients

get_siera_coefficients <- function(){

  library(XML)
  library(dplyr)

  siera.data <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=70&type=c,6,14,19,24,25,26,7,8&season=2014&month=0&season1=2003&ind=1&team=0&rost=0&age=0&filter=&players=0&page=1_4000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00[,2:12]
  for (i in c(1,4:11)){
    class(siera.data[,i]) <- "numeric"
  }

  siera.data <- siera.data %>% mutate(so.pa = SO/TBF) %>% mutate(so.pa.sq = (SO/TBF)^2) %>%
    mutate(bb.pa = BB/TBF) %>% mutate(bb.pa.sq = (BB/TBF)^2) %>% mutate(netgb.pa = (GB - FB)/TBF) %>%
    mutate(netgb.pa.sq = ((GB - FB)/abs(GB - FB))*((GB - FB)/TBF)^2)

  siera.data <- siera.data %>% mutate(so.pa_bb.pa = so.pa*bb.pa) %>% mutate(so.pa_netgb.pa = so.pa*netgb.pa) %>%
    mutate(bb.pa_netgb.pa = bb.pa*netgb.pa) %>% mutate(gs.percent = GS/G)

  reg <- with(siera.data, lm(ERA ~ so.pa + so.pa.sq + bb.pa + netgb.pa + netgb.pa.sq + so.pa_netgb.pa + gs.percent))

  return(reg)
}
