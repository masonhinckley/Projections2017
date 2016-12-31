
wOBA_statcast_est_2016 <- function(glm.fit, park.factors){

  data("sd2016")
  data("wOBA_constants")

  sd2016 <- sd2016 %>% mutate(cf = ifelse(hit_location == 8, 1, 0)) %>%
    mutate(lf = ifelse(hit_location == 7, 1, 0)) %>%
    mutate(rf = ifelse(hit_location == 9, 1, 0)) %>%
    mutate(single = ifelse(events == "Single", 1, 0)) %>%
    mutate(double = ifelse(events == "Double", 1, 0)) %>%
    mutate(triple = ifelse(events == "Triple", 1, 0)) %>%
    mutate(homerun = ifelse(events == "Home Run", 1, 0))

  sd2016 <- mutate(sd2016, out = ifelse(single + double + triple + homerun == 0, 1, 0))

  sd2016$hit_speed <- as.numeric(sd2016$hit_speed)
  sd2016$hit_angle <- as.numeric(sd2016$hit_angle)

  sd2016 <- sd2016 %>% mutate(hit_speed_sq = hit_speed^2) %>% mutate(hit_angle_sq = hit_angle^2) %>%
    mutate(hit_speed_cu = hit_speed^3)

  fg.stats.2016 <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,4,6,11,12,13,21,-1,34,35,40,41,-1,23,37,38,50,61,-1,111,-1,203,199,58,60&season=2016&month=0&season1=2016&ind=0&team=&rost=&age=&filter=&players=&page=1_1000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  fg.stats.spd.2016 <- fg.stats.2016[,c(2,23)]
  fg.stats.spd.2016$Spd <- as.numeric(fg.stats.spd.2016$Spd)
  hitter.codes <- player_codes
  hitter.codes <- hitter.codes[,1:2]
  sd2016 <- left_join(sd2016, hitter.codes, by = c("batter" = "mlb_id"))
  sd2016 <- left_join(sd2016, fg.stats.spd.2016, by = c("player_name.y" = "Name"))
  sd2016$game_date <- as.Date(sd2016$game_date)
  sd2016 <- filter(sd2016, !is.na(Spd))
  sd2016 <- filter(sd2016, !is.na(player_name.x))

  testdata <- predict(glm.fit, sd2016, "probs")
  sd2016 <- cbind(sd2016, testdata)

  for (i in 1:nrow(sd2016)){
    if (is.na(sd2016$Single[i])){
      if (sd2016$single[i] == 1){
        sd2016$Single[i] = 1
      }
      else{
        sd2016$Single[i] = 0
      }
    }

    if (is.na(sd2016$Double[i])){
      if (sd2016$double[i] == 1){
        sd2016$Double[i] = 1
      }
      else{
        sd2016$Double[i] = 0
      }
    }

    if (is.na(sd2016$Triple[i])){
      if (sd2016$triple[i] == 1){
        sd2016$Triple[i] = 1
      }
      else{
        sd2016$Triple[i] = 0
      }
    }

    if (is.na(sd2016$Homerun[i])){
      if (sd2016$homerun[i] == 1){
        sd2016$Homerun[i] = 1
      }
      else{
        sd2016$Homerun[i] = 0
      }
    }
  }

  wOBA.fg.data <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,14,15,17,18,8,9,10,11,5,6,50&season=2016&month=0&season1=2016&ind=0&team=&rost=&age=0&filter=&players=&page=1_1000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00

  for(i in 4:14){
    class(wOBA.fg.data[,i]) <- "numeric"
  }


  wOBA.test.table <- sd2016 %>% group_by(player_name.y, hit_location, home_team) %>%
    summarize(xpa1B = sum(Single), xpa2B = sum(Double), xpa3B = sum(Triple), xpaHR = sum(Homerun))

  wOBA.test.table <- wOBA.test.table[complete.cases(wOBA.test.table),]

  wOBA.test.table <- left_join(wOBA.test.table, park.factors)

  wOBA.test.table$xHR <- wOBA.test.table$xpaHR*wOBA.test.table$pf_HR
  wOBA.test.table$x3B <- wOBA.test.table$xpa3B*wOBA.test.table$pf_3B
  wOBA.test.table$x2B <- wOBA.test.table$xpa2B*wOBA.test.table$pf_2B
  wOBA.test.table$x1B <- wOBA.test.table$xpa1B*wOBA.test.table$pf_1B

  wOBA.test.table <- wOBA.test.table %>% group_by(player_name.y) %>% summarise(x1B = sum(x1B), x2B = sum(x2B), x3B = sum(x3B), xHR = sum(xHR), xpa1B = sum(xpa1B), xpa2B = sum(xpa2B), xpa3B = sum(xpa3B), xpaHR = sum(xpaHR))

  wOBA.test.table <- left_join(wOBA.fg.data, wOBA.test.table, c("Name" = "player_name.y"))

  wOBA.constants <- wOBA_constants
  wOBA.constants <- filter(wOBA.constants, Season == 2016)

  wBB <- wOBA.constants$wBB
  wHBP <- wOBA.constants$wHBP
  w1B <- wOBA.constants$w1B
  w2B <- wOBA.constants$w2B
  w3B <- wOBA.constants$w3B
  wHR <- wOBA.constants$wHR

  wOBA.test.table <- wOBA.test.table %>%
    mutate(xwOBA = (wBB*BB + wHBP*HBP + w1B*x1B + w2B*x2B + w3B*x3B + wHR*xHR)/(AB + BB - IBB + SF + HBP)) %>%
    mutate(xpawOBA = (wBB*(BB - IBB) + wHBP*HBP + w1B*xpa1B + w2B*xpa2B + w3B*xpa3B + wHR*xpaHR)/(AB + BB - IBB + SF + HBP)) %>%
    mutate(diff = xwOBA - wOBA) %>% mutate(b3.diff = xpa3B - `3B`) %>% mutate(b2.diff = xpa2B - `2B`) %>% mutate(hr.diff = xpaHR - HR)

  wOBA.test.table <- wOBA.test.table[complete.cases(wOBA.test.table),]

  return(wOBA.test.table)

}
