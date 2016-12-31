### Park Factors

get_hitter_park_factors <- function(){

  library(dplyr)
  # library(MBACprojections)

  data("sd2016")
  data("player_codes")

  raw.data <- sd2016 %>% mutate(cf = ifelse(hit_location == 8, 1, 0)) %>%
    mutate(lf = ifelse(hit_location == 7, 1, 0)) %>%
    mutate(rf = ifelse(hit_location == 9, 1, 0)) %>%
    mutate(single = ifelse(events == "Single", 1, 0)) %>%
    mutate(double = ifelse(events == "Double", 1, 0)) %>%
    mutate(triple = ifelse(events == "Triple", 1, 0)) %>%
    mutate(homerun = ifelse(events == "Home Run", 1, 0))


  sorted.data <- raw.data %>% group_by(home_team, hit_location) %>% summarize(HR = sum(homerun), b3 = sum(triple), b2 = sum(double), b1 = sum(single))

  logit.data <- setup_logit()

  single.logit <- glm(single ~ hit_speed + hit_speed_sq + hit_angle + hit_angle_sq + hit_speed_cu + Spd + rf + lf, family = binomial(link ="logit"), data = logit.data)
  double.logit <- glm(double ~ hit_speed + hit_speed_sq + hit_angle + hit_angle_sq + hit_speed_cu + Spd + rf + lf, family = binomial(link ="logit"), data = logit.data)
  triple.logit <- glm(triple ~ hit_speed + hit_speed_sq + hit_angle + hit_angle_sq + hit_speed_cu + Spd + rf + lf, family = binomial(link ="logit"), data = logit.data)
  homerun.logit <- glm(homerun ~ hit_speed + hit_speed_sq + hit_speed_cu + hit_angle + hit_angle_sq + rf + lf, family = binomial(link ="logit"), data = logit.data)

  raw.data$hit_speed <- as.numeric(raw.data$hit_speed)
  raw.data$hit_angle <- as.numeric(raw.data$hit_angle)

  raw.data <- raw.data %>% mutate(hit_speed_sq = hit_speed^2) %>% mutate(hit_angle_sq = hit_angle^2) %>%
    mutate(hit_speed_cu = hit_speed^3)

  fg.stats.2016 <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,4,6,11,12,13,21,-1,34,35,40,41,-1,23,37,38,50,61,-1,111,-1,203,199,58,60&season=2016&month=0&season1=2016&ind=0&team=&rost=&age=&filter=&players=&page=1_1000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  fg.stats.spd.2016 <- fg.stats.2016[,c(2,23)]
  fg.stats.spd.2016$Spd <- as.numeric(fg.stats.spd.2016$Spd)
  hitter.codes <- player_codes
  hitter.codes <- hitter.codes[,1:2]
  raw.data <- left_join(raw.data, hitter.codes, by = c("batter" = "mlb_id"))
  raw.data <- left_join(raw.data, fg.stats.spd.2016, by = c("player_name.y" = "Name"))
  raw.data$game_date <- as.Date(raw.data$game_date)
  mn.spd <- mean(fg.stats.spd.2016$Spd)

  raw.data$Spd[is.na(raw.data$Spd)] <- mn.spd

  raw.data <- raw.data %>% mutate(single_logit = predict(single.logit, raw.data, type = "response")) %>%
    mutate(double_logit = predict(double.logit, raw.data, type = "response")) %>%
    mutate(triple_logit = predict(triple.logit, raw.data, type = "response")) %>%
    mutate(homerun_logit = predict(homerun.logit, raw.data, type = "response"))

  for (i in 1:nrow(raw.data)){
    if (is.na(raw.data$single_logit[i]) || raw.data$hit_speed[i] == 0){
      if (raw.data$single[i] == 1){
        raw.data$single_logit[i] = 1
        raw.data$double_logit[i] = 0
        raw.data$triple_logit[i] = 0
        raw.data$homerun_logit[i] = 0
      }
      else if (raw.data$double[i] == 1){
        raw.data$single_logit[i] = 0
        raw.data$double_logit[i] = 1
        raw.data$triple_logit[i] = 0
        raw.data$homerun_logit[i] = 0
      }
      else if (raw.data$triple[i] == 1){
        raw.data$single_logit[i] = 0
        raw.data$double_logit[i] = 0
        raw.data$triple_logit[i] = 1
        raw.data$homerun_logit[i] = 0
      }
      else if (raw.data$homerun[i] == 1){
        raw.data$single_logit[i] = 0
        raw.data$double_logit[i] = 0
        raw.data$triple_logit[i] = 0
        raw.data$homerun_logit[i] = 1
      }
      else{
        raw.data$single_logit[i] = 0
        raw.data$double_logit[i] = 0
        raw.data$triple_logit[i] = 0
        raw.data$homerun_logit[i] = 0
      }
    }
  }

  sorted.statcast.data <- raw.data %>% group_by(home_team, hit_location) %>% summarise(x1B = sum(single_logit), x2B = sum(double_logit), x3B = sum(triple_logit), xHR = sum(homerun_logit))

  sorted.data <- left_join(sorted.data, sorted.statcast.data)

  sorted.data <- sorted.data %>% mutate(pf_1B = (b1/x1B)) %>% mutate(pf_2B = (b2/x2B)) %>% mutate(pf_3B = (b3/x3B)) %>% mutate(pf_HR = (HR/xHR))

  sorted.data <- sorted.data[,c(1:2,11:14)]

  sorted.data[is.na(sorted.data)] <- 1

  return(sorted.data)

}
