### Obtain season projections for every pitcher with prior stats within 2013-2015

## IMPROVEMENTS:
## Apply aging to each component statistic of SIERA rather than just ERA aging
## Project each pitchers starting and relief pitching performance seperately, then combine for ERA, WAR proj.
## Project rookies to be league average rookies rather than league average overall


season_projections <- function(past.statistics, project.year, siera.reg, sp.aging.factors, rp.aging.factors){

  library(dplyr)
  data("pitcherProjections2017")

  past.statistics <- raw.data
  project.year <- 2017
  siera.reg <- reg
  sp.aging.factors <- sp.aging.factors
  rp.aging.factors <- rp.aging.factors

  past.season <- project.year - 1
  second.season <- project.year - 2
  third.season <- project.year - 3

  stats.past.year <- filter(past.statistics, Season == past.season)
  stats.second.season <- filter(past.statistics, Season == second.season)
  stats.third.season <- filter(past.statistics, Season == third.season)

  stats.past.year$Age <- stats.past.year$Age + 1
  stats.second.season$Age <- stats.second.season$Age + 2
  stats.third.season$Age <- stats.third.season$Age + 3

  al.vect <- c("Orioles", "Blue Jays", "Red Sox", "Yankees", "Rays", "Indians", "Tigers", "White Sox", "Royals", "Twins", "Rangers", "Mariners", "Angels", "Athletics", "Astros")

  lg.averages <- data.frame(BB. = sum(stats.past.year$BB)/sum(stats.past.year$TBF), K. = sum(stats.past.year$SO)/sum(stats.past.year$TBF),
                            FB. = sum(stats.past.year$FB)/(sum(stats.past.year$TBF) - sum(stats.past.year$BB) - sum(stats.past.year$IBB) - sum(stats.past.year$HBP) - sum(stats.past.year$SO)),
                            GB. = sum(stats.past.year$GB)/(sum(stats.past.year$TBF) - sum(stats.past.year$BB) - sum(stats.past.year$IBB) - sum(stats.past.year$HBP) - sum(stats.past.year$SO)),
                            BIP. = (sum(stats.past.year$TBF) - sum(stats.past.year$BB) - sum(stats.past.year$IBB) - sum(stats.past.year$HBP) - sum(stats.past.year$SO))/sum(stats.past.year$TBF))

  age.frame <- unique(rbind(stats.past.year, stats.second.season, stats.third.season)[,c(2,4)])

  final.frame <- data.frame(Name = character(), K. = numeric(), BB. = numeric())

  for(i in 1:nrow(age.frame)){

    player <- age.frame$Name[i]
    player.age <- age.frame$Age[i]

    if(player %in% stats.past.year$Name){
      index <- match(player, stats.past.year$Name)
      temp.data <- stats.past.year[index,]
      player.data <- data.frame(Name = temp.data$Name, Season = temp.data$Season, Age = player.age,
                                TBF = temp.data$TBF, SO = temp.data$SO, BB = temp.data$BB, HBP = temp.data$HBP,
                                IBB = temp.data$IBB, GB = temp.data$GB, FB = temp.data$FB, weight = 3)
    } else {
      player.data <- data.frame(Name = player, Season = past.season, Age = player.age,
                                TBF = 0, SO = 0, BB = 0, HBP = 0, IBB = 0,
                                GB = 0, FB = 0, weight = 3)
    }

    if(player %in% stats.second.season$Name){
      index <- match(player, stats.second.season$Name)
      temp.data <- stats.second.season[index,]
      player.data.2 <- data.frame(Name = temp.data$Name, Season = temp.data$Season, Age = player.age,
                                  TBF = temp.data$TBF, SO = temp.data$SO, BB = temp.data$BB,
                                  HBP = temp.data$HBP,
                                  IBB = temp.data$IBB, GB = temp.data$GB, FB = temp.data$FB, weight = 2)
    } else{
      player.data.2 <- data.frame(Name = player, Season = second.season, Age = player.age,
                                  TBF = 0, SO = 0, BB = 0, HBP = 0, IBB = 0,
                                  GB = 0, FB = 0, weight = 2)
    }

    if(player %in% stats.third.season$Name){
      index <- match(player, stats.third.season$Name)
      temp.data <- stats.third.season[index,]
      player.data.3 <- data.frame(Name = temp.data$Name, Season = temp.data$Season, Age = player.age,
                                  TBF = temp.data$TBF, SO = temp.data$SO, BB = temp.data$BB, HBP = temp.data$HBP,
                                  IBB = temp.data$IBB, GB = temp.data$GB, FB = temp.data$FB, weight = 1)
    } else{
      player.data.3 <- data.frame(Name = player, Season = third.season, Age = player.age,
                                  TBF = 0, SO = 0, BB = 0, HBP = 0, IBB = 0,
                                  GB = 0, FB = 0, weight = 1)
    }

    temp.frame  <- rbind(player.data, player.data.2, player.data.3)

    temp.frame <- temp.frame %>% mutate(HBP. = HBP/TBF) %>% mutate(K. = SO/TBF) %>% mutate(BB. = BB/TBF) %>%
      mutate(FB. = FB/(TBF - BB - IBB - HBP - SO)) %>% mutate(GB. = GB/(TBF - BB - IBB - HBP - SO)) %>%
      mutate(BIP. = (TBF - BB - IBB - HBP - SO)/TBF) %>% mutate(eBIP = 0)

    temp.frame[is.na(temp.frame)] <- 0

    mean.TBF <- sum(temp.frame$TBF)/nrow(temp.frame)

    temp.frame <- mutate(temp.frame, weight_ratio = TBF/mean.TBF)

    temp.frame[is.na(temp.frame)] <- 0

    temp.frame <- mutate(temp.frame, weight = weight*weight_ratio)

    temp.frame$HBP. <- temp.frame$HBP.*temp.frame$weight
    temp.frame$K. <- temp.frame$K.*temp.frame$weight
    temp.frame$BB. <- temp.frame$BB.*temp.frame$weight
    temp.frame$BIP. <- temp.frame$BIP.*temp.frame$weight
    temp.frame$FB. <- temp.frame$FB.*temp.frame$weight
    temp.frame$GB. <- temp.frame$GB.*temp.frame$weight

    temp.frame.2 <- data.frame(Name = player, Age = median(temp.frame$Age), TBF = sum(temp.frame$TBF) ,HBP. = sum(temp.frame$HBP.)/sum(temp.frame$weight), K. = sum(temp.frame$K.)/sum(temp.frame$weight),
                               BB. = sum(temp.frame$BB.)/sum(temp.frame$weight), BIP. = sum(temp.frame$BIP.)/sum(temp.frame$weight), FB. = sum(temp.frame$FB.)/sum(temp.frame$weight),
                               GB. = sum(temp.frame$GB.)/sum(temp.frame$weight))

    temp.frame.3 <- data.frame(Name = player, Age = temp.frame.2$Age, K. = ((temp.frame.2$K.*temp.frame.2$TBF) + (lg.averages$K.*70))/(sum(temp.frame.2$TBF) + 70),
                               BB. = ((temp.frame.2$BB.*temp.frame.2$TBF) + (lg.averages$BB.*170))/(sum(temp.frame.2$TBF) + 170),
                               BIP. = ((temp.frame.2$BIP.*temp.frame.2$TBF) + (lg.averages$BIP.*60))/(sum(temp.frame.2$TBF) + 60),
                               GB. = ((temp.frame.2$GB.*(temp.frame.2$TBF*temp.frame.2$BIP.)) + (lg.averages$GB.*70))/((temp.frame.2$TBF*temp.frame.2$BIP.) + 70),
                               FB. = ((temp.frame.2$FB.*(temp.frame.2$TBF*temp.frame.2$BIP.)) + (lg.averages$FB.*70))/((temp.frame.2$TBF*temp.frame.2$BIP.) + 70))

    final.frame <- rbind(final.frame, temp.frame.3)

  }

  final.frame <- final.frame[order(-final.frame$Age),]

  final.frame <- final.frame[!duplicated(final.frame$Name),]

  dc.2017 <- pitcherProjections2017
  dc.2017 <- mutate(dc.2017, dcERA = ERA)
  dc.2017 <- dc.2017[,c(1:2,8:10,22)]

  final.frame <- left_join(dc.2017, final.frame, by = c("Name" = "Name"))

  final.frame <- final.frame[complete.cases(final.frame),]

  final.frame <- final.frame %>% mutate(so.pa = K.) %>% mutate(so.pa.sq = (K.)^2) %>%
    mutate(bb.pa = BB.) %>% mutate(bb.pa.sq = (BB.)^2) %>% mutate(netgb.pa = (GB.*BIP.) - (FB.*BIP.)) %>%
    mutate(netgb.pa.sq = ((GB.*BIP. - FB.*BIP.)/abs(GB.*BIP. - FB.*BIP.))*((GB.*BIP. - FB.*BIP.)^2))

  final.frame <- final.frame %>% mutate(so.pa_bb.pa = so.pa*bb.pa) %>% mutate(so.pa_netgb.pa = so.pa*netgb.pa) %>%
    mutate(bb.pa_netgb.pa = bb.pa*netgb.pa) %>% mutate(gs.percent = GS/G)

  final.frame <- mutate(final.frame, xSIERA = predict(siera.reg, final.frame, type = "response"))

  league.data <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,13,16,14,17,19,24,25,26,7,8&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_1000"), stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  for(i in c(1,4:13)){
    class(league.data[,i]) <- "numeric"
  }
  lg.avg.era <- (sum(league.data$ER)/sum(league.data$IP))*9

  league.data <- mutate(league.data, League = ifelse(Team %in% al.vect, "AL", "NL"))
  al.avg.era <- filter(league.data, League == "AL")
  al.avg.era <- (sum(al.avg.era$ER)/sum(al.avg.era$IP))*9

  nl.avg.era <- filter(league.data, League == "NL")
  nl.avg.era <- (sum(nl.avg.era$ER)/sum(nl.avg.era$IP))*9

  avg.siera.frame <- data.frame(TBF = sum(league.data$TBF), BB = sum(league.data$BB), SO = sum(league.data$SO),
                                GB = sum(league.data$GB), FB = sum(league.data$FB), G = sum(league.data$G),
                                GS = sum(league.data$GS))
  avg.siera.frame <- avg.siera.frame %>% mutate(so.pa = SO/TBF) %>% mutate(so.pa.sq = (SO/TBF)^2) %>%
    mutate(bb.pa = BB/TBF) %>% mutate(bb.pa.sq = (BB/TBF)^2) %>% mutate(netgb.pa = (GB - FB)/TBF) %>%
    mutate(netgb.pa.sq = ((GB - FB)/abs(GB - FB))*((GB - FB)/TBF)^2)

  avg.siera.frame <- avg.siera.frame %>% mutate(so.pa_bb.pa = so.pa*bb.pa) %>% mutate(so.pa_netgb.pa = so.pa*netgb.pa) %>%
    mutate(bb.pa_netgb.pa = bb.pa*netgb.pa) %>% mutate(gs.percent = GS/G)

  avg.siera.frame <- mutate(avg.siera.frame, xSIERA = predict(siera.reg, avg.siera.frame, type = "response"))

  lg.avg.siera <- avg.siera.frame$xSIERA

  final.frame <- final.frame %>% mutate(xSIERA = xSIERA*(lg.avg.era/lg.avg.siera)) %>%
    mutate(GS.percent = GS/G)

  for (i in 1:nrow(final.frame)){
    if (final.frame$GS.percent[i] >= .7){
      age <- final.frame$Age[i]
      aging.factor <- filter(sp.aging.factors, post.age == age)[,c("ERA.aging")]
      final.frame$xSIERA[i] <- final.frame$xSIERA[i] + aging.factor
    }
    else{
      age <- final.frame$Age[i]
      aging.factor <- filter(rp.aging.factors, post.age == age)[,c("ERA.aging")]
      final.frame$xSIERA[i] <- final.frame$xSIERA[i] + aging.factor
    }
  }

  final.frame <- mutate(final.frame, League = ifelse(Team %in% al.vect, "AL", "NL"))

  rp.replacement <- .470
  sp.replacement <- .380

  runs.per.game <- (sum(league.data$R)/2430)/2
  earnedruns.to.runs <- sum(league.data$ER)/sum(league.data$R)

  al.team.stats <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=al&qual=0&type=8&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0,ts&rost=0&age=0&filter=&players=0"), stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  nl.team.stats <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=nl&qual=0&type=8&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0,ts&rost=0&age=0&filter=&players=0"), stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  al.team.stats$R <- as.numeric(al.team.stats$R)
  nl.team.stats$R <- as.numeric(nl.team.stats$R)

  al.runs.per.game <- (sum(al.team.stats$R)/1215)/2
  nl.runs.per.game <- (sum(nl.team.stats$R)/1215)/2

  final.frame <- mutate(final.frame, adj.rpg.siera = ifelse(League == "AL", xSIERA*(al.runs.per.game/al.avg.era), xSIERA*(nl.runs.per.game/nl.avg.era)))

  final.frame <- mutate(final.frame, ip.per.g = IP/G)
  final.frame <- mutate(final.frame, runs.per.win = ifelse(League == "AL", (((((18-ip.per.g)*al.runs.per.game) + (ip.per.g*adj.rpg.siera))/18) + 2)*1.5, (((((18-ip.per.g)*nl.runs.per.game) + (ip.per.g*adj.rpg.siera))/18) + 2)*1.5))

  final.frame <- mutate(final.frame, win.percent = ifelse(League == "AL", ((al.runs.per.game - adj.rpg.siera)/runs.per.win) + .5, ((nl.runs.per.game - adj.rpg.siera)/runs.per.win) + .5))

  final.frame <- mutate(final.frame, WAR = ifelse(gs.percent >= .5, (win.percent - .38)*(IP/9), (win.percent - .47)*(IP/9)))

  pf.2016 <- readHTMLTable("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2015", stringsAsFactors = FALSE)$GutsBoard1_dg1_ctl00
  for (i in c(1,3:14)){
    class(pf.2016[,i]) <- "numeric"
  }
  pf.2016[,3:14] <- pf.2016[,3:14]/100

  final.frame <- left_join(final.frame, pf.2016, by = c("Team" = "Team"))

  final.frame[is.na(final.frame)] <- 1

  final.frame <- final.frame %>% mutate(BIP. = BIP. - (K.*SO - K.) - (BB.*BB - BB.))

  final.frame <- final.frame %>% mutate(K. = K.*SO) %>% mutate(BB. = BB.*BB) %>% mutate(GB. = GB.*GB) %>%
    mutate(FB. = FB.*FB)

  final.frame <- final.frame %>% mutate(so.pa = K.) %>% mutate(so.pa.sq = (K.)^2) %>%
    mutate(bb.pa = BB.) %>% mutate(bb.pa.sq = (BB.)^2) %>% mutate(netgb.pa = (GB.*BIP.) - (FB.*BIP.)) %>%
    mutate(netgb.pa.sq = ((GB.*BIP. - FB.*BIP.)/abs(GB.*BIP. - FB.*BIP.))*((GB.*BIP. - FB.*BIP.)^2))

  final.frame <- final.frame %>% mutate(so.pa_bb.pa = so.pa*bb.pa) %>% mutate(so.pa_netgb.pa = so.pa*netgb.pa) %>%
    mutate(bb.pa_netgb.pa = bb.pa*netgb.pa) %>% mutate(gs.percent = GS/G)

  final.frame <- mutate(final.frame, xSIERA = predict(siera.reg, final.frame, type = "response"))

  league.data <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,13,16,14,17,19,24,25,26,7,8&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_1000"), stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  for(i in c(1,4:13)){
    class(league.data[,i]) <- "numeric"
  }
  lg.avg.era <- (sum(league.data$ER)/sum(league.data$IP))*9

  league.data <- mutate(league.data, League = ifelse(Team %in% al.vect, "AL", "NL"))
  al.avg.era <- filter(league.data, League == "AL")
  al.avg.era <- (sum(al.avg.era$ER)/sum(al.avg.era$IP))*9

  nl.avg.era <- filter(league.data, League == "NL")
  nl.avg.era <- (sum(nl.avg.era$ER)/sum(nl.avg.era$IP))*9

  avg.siera.frame <- data.frame(TBF = sum(league.data$TBF), BB = sum(league.data$BB), SO = sum(league.data$SO),
                                GB = sum(league.data$GB), FB = sum(league.data$FB), G = sum(league.data$G),
                                GS = sum(league.data$GS))
  avg.siera.frame <- avg.siera.frame %>% mutate(so.pa = SO/TBF) %>% mutate(so.pa.sq = (SO/TBF)^2) %>%
    mutate(bb.pa = BB/TBF) %>% mutate(bb.pa.sq = (BB/TBF)^2) %>% mutate(netgb.pa = (GB - FB)/TBF) %>%
    mutate(netgb.pa.sq = ((GB - FB)/abs(GB - FB))*((GB - FB)/TBF)^2)

  avg.siera.frame <- avg.siera.frame %>% mutate(so.pa_bb.pa = so.pa*bb.pa) %>% mutate(so.pa_netgb.pa = so.pa*netgb.pa) %>%
    mutate(bb.pa_netgb.pa = bb.pa*netgb.pa) %>% mutate(gs.percent = GS/G)

  avg.siera.frame <- mutate(avg.siera.frame, xSIERA = predict(siera.reg, avg.siera.frame, type = "response"))

  lg.avg.siera <- avg.siera.frame$xSIERA

  final.frame <- final.frame %>% mutate(xSIERA = xSIERA*(lg.avg.era/lg.avg.siera)) %>%
    mutate(GS.percent = GS/G)

  for (i in 1:nrow(final.frame)){
    if (final.frame$GS.percent[i] >= .7){
      age <- final.frame$Age[i]
      aging.factor <- filter(sp.aging.factors, post.age == age)[,c("ERA.aging")]
      final.frame$xSIERA[i] <- final.frame$xSIERA[i] + aging.factor
    }
    else{
      age <- final.frame$Age[i]
      aging.factor <- filter(rp.aging.factors, post.age == age)[,c("ERA.aging")]
      final.frame$xSIERA[i] <- final.frame$xSIERA[i] + aging.factor
    }
  }

  final.frame <- final.frame[,1:30]

  return(final.frame)

}
