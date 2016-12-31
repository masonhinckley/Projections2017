### Setup data for probit model

setup_logit <- function(){

  library(dplyr)
  library(ggplot2)
  library(XML)
  # library(MBACprojections)

  data("sd2015")
  data("player_codes")

  raw.data <- sd2015 %>% mutate(cf = ifelse(hit_location == 8, 1, 0)) %>%
    mutate(lf = ifelse(hit_location == 7, 1, 0)) %>%
    mutate(rf = ifelse(hit_location == 9, 1, 0)) %>%
    mutate(single = ifelse(events == "Single", 1, 0)) %>%
    mutate(double = ifelse(events == "Double", 1, 0)) %>%
    mutate(triple = ifelse(events == "Triple", 1, 0)) %>%
    mutate(homerun = ifelse(events == "Home Run", 1, 0))

  raw.data <- mutate(raw.data, out = ifelse(single + double + triple + homerun == 0, 1, 0))

  raw.data <- mutate(raw.data, outcome = ifelse(single == 1, "Single", "Out"))
  raw.data <- raw.data %>% mutate(outcome = ifelse(double == 1, "Double", outcome)) %>%
    mutate(outcome = ifelse(triple == 1, "Triple", outcome)) %>% mutate(outcome = ifelse(homerun == 1, "Homerun", outcome))

  raw.data <- raw.data %>% filter(hit_speed != "null" & hit_angle != "null")

  raw.data$hit_speed <- as.numeric(raw.data$hit_speed)
  raw.data$hit_angle <- as.numeric(raw.data$hit_angle)

  raw.data <- raw.data %>% mutate(hit_speed_sq = hit_speed^2) %>% mutate(hit_angle_sq = hit_angle^2) %>%
    mutate(hit_speed_cu = hit_speed^3)

  fg.stats <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,4,6,11,12,13,21,-1,34,35,40,41,-1,23,37,38,50,61,-1,111,-1,203,199,58,60&season=2015&month=0&season1=2015&ind=0&team=&rost=&age=&filter=&players=&page=1_1500", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  class(fg.stats[,23]) <- "numeric"
  fg.stats.spd <- fg.stats[,c(2,23)]
  hitter.codes <- player_codes
  hitter.codes <- hitter.codes[,1:2]
  raw.data <- full_join(raw.data, hitter.codes, by = c("batter" = "mlb_id"))
  raw.data <- full_join(raw.data, fg.stats.spd, by = c("player_name.y" = "Name"))

  raw.data <- raw.data[complete.cases(raw.data),]
  raw.data <- filter(raw.data, hit_speed != 0)
  raw.data$game_date <- as.Date(raw.data$game_date)

  return(raw.data)

}
