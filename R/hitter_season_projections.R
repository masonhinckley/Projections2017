

hitter_season_projections <- function(projection.data, wOBA.test.table, projection.year, hitter.aging.factors){

  library(dplyr)
  library(XML)
  data("wOBA_constants")

  projection.year <- 2017
  past.season <- projection.year - 1
  second.season <- projection.year - 2
  third.season <- projection.year - 3

  statcast.age.frame <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,3&season=2016&month=0&season1=2016&ind=0&team=&rost=&age=&filter=&players=&page=1_1000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  statcast.age.frame <- statcast.age.frame[,c(2,4)]
  class(statcast.age.frame$Age) <- "numeric"

  statcast.replacement.data <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,5,8,9,10,11,14,15,17,18,111,53,54,55,56,204,199&season=2016&month=0&season1=2016&ind=1&team=&rost=&age=&filter=&players=&page=1_1000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  for (i in c(1,2,5:21)){
    class(statcast.replacement.data[,i]) <- "numeric"
  }

  past.statistics <- projection.data
  class(past.statistics$Season) <- "numeric"

  statcast.data <- wOBA.test.table

  statcast.data <- statcast.data %>% mutate(HBP. = HBP/PA) %>% mutate(BB. = BB/PA) %>%
    mutate(IBB. = IBB/PA) %>% mutate(SF. = SF/PA) %>% mutate(HR. = xpaHR/PA) %>% mutate(b3. = xpa3B/PA) %>%
    mutate(b2. = xpa2B/PA) %>% mutate(b1. = xpa1B/PA)

  past.statistics <- past.statistics %>% filter(Season < 2016)

  statcast.data <- mutate(statcast.data, Season = 2016)

  fg.data.2016 <- statcast.replacement.data %>% mutate(bat. = Bat/PA) %>% mutate(fld. = Fld/PA) %>%
    mutate(bsr. = BsR/PA)

  fg.data.2016 <- fg.data.2016[,c(3,15:17,22:24)]

  statcast.data <- left_join(statcast.data, fg.data.2016, by = c("Name" = "Name"))

  statcast.data <- statcast.data[,c(2:13, 29:43)]

  statcast.data <- left_join(statcast.data, statcast.age.frame, by = c("Name" = "Name"))

  past.statistics <- rbind(past.statistics, statcast.data)

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

  past.statistics <- left_join(past.statistics, fgpf, by = c("Team" = "Team", "Season" = "Season"))
  past.statistics[is.na(past.statistics)] <- 1

  names(past.statistics)[7:11] <- c("1B", "2B", "3B", "HR", "BB")

  past.statistics <- past.statistics %>% mutate(`2B.y` = ifelse(Season == 2016, 1, `2B.y`)) %>%
    mutate(`3B.y` = ifelse(Season == 2016, 1, `3B.y`)) %>% mutate(HR.y = ifelse(Season == 2016, 1, HR.y))

  past.statistics <- mutate(past.statistics, `1B.y` = ifelse(Season == 2016, 1, `1B.y`))

  past.statistics <- past.statistics %>% mutate(b1. = b1./`1B.y`) %>% mutate(b2. = b2./`2B.y`) %>%
    mutate(b3. = b3./`3B.y`) %>% mutate(HR. = HR./HR.y) %>% mutate(BB. = BB./BB.y)

  past.statistics <- past.statistics[,1:28]

  stats.past.season <- filter(past.statistics, Season == past.season)
  stats.second.season <- filter(past.statistics, Season == second.season)
  stats.third.season <- filter(past.statistics, Season == third.season)

  stats.past.season$Age <- stats.past.season$Age + 1
  stats.second.season$Age <- stats.second.season$Age + 2
  stats.third.season$Age <- stats.third.season$Age + 3

  lg.averages <- data.frame(HBP. = sum(statcast.replacement.data$HBP)/sum(statcast.replacement.data$PA),
                            BB. = sum(statcast.replacement.data$BB)/sum(statcast.replacement.data$PA),
                            IBB. = sum(statcast.replacement.data$IBB)/sum(statcast.replacement.data$PA),
                            SF. = sum(statcast.replacement.data$SF)/sum(statcast.replacement.data$PA),
                            HR. = sum(statcast.replacement.data$HR)/sum(statcast.replacement.data$PA),
                            b3. = sum(statcast.replacement.data$`3B`)/sum(statcast.replacement.data$PA),
                            b2. = sum(statcast.replacement.data$`2B`)/sum(statcast.replacement.data$PA),
                            b1. = sum(statcast.replacement.data$`1B`)/sum(statcast.replacement.data$PA),
                            bat. = 0, fld. = 0, bsr. = 0)

  age.frame <- unique(rbind(stats.past.season, stats.second.season, stats.third.season)[,c(2,4)])

  final.frame <- data.frame(Name = character(), Age = numeric(), Team = character(), HBP. = numeric(), BB. = numeric(),
                            IBB. = numeric(), SF. = numeric(), HR. = numeric(), b3. = numeric(),
                            b2. = numeric(), b1. = numeric(), bat. = numeric(), fld. = numeric(),
                            bsr. = numeric())

  for(i in 1:nrow(age.frame)){

    player <- age.frame$Name[i]
    player.age <- age.frame$Age[i]

    if(player %in% stats.past.season$Name){
      index <- match(player, stats.past.season$Name)
      temp.data <- stats.past.season[index,]
      player.data <- temp.data[,c(1:2,4:5,18:28)]
      player.data$weight <- 5
    } else{
      player.data <- data.frame(Season = past.season, Name = player, Age = player.age,
                                PA = 0, HBP. = 0, BB. = 0, IBB. = 0, SF. = 0,
                                HR. = 0, b3. = 0, b2. = 0, b1. = 0, bat. = 0, fld. = 0, bsr. = 0, weight = 5)
    }

    if(player %in% stats.second.season$Name){
      index <- match(player, stats.second.season$Name)
      temp.data <- stats.second.season[index,]
      player.data.2 <- temp.data[,c(1:2,4:5,18:28)]
      player.data.2$weight <- 4
    } else{
      player.data.2 <- data.frame(Season = second.season, Name = player, Age = player.age,
                                  PA = 0, HBP. = 0, BB. = 0, IBB. = 0, SF. = 0,
                                  HR. = 0, b3. = 0, b2. = 0, b1. = 0, bat. = 0, fld. = 0, bsr. = 0, weight = 4)
    }

    if(player %in% stats.third.season$Name){
      index <- match(player, stats.third.season$Name)
      temp.data <- stats.third.season[index,]
      player.data.3 <- temp.data[,c(1:2,4:5,18:28)]
      player.data.3$weight <- 3
    } else{
      player.data.3 <- data.frame(Season = third.season, Name = player, Age = player.age,
                                  PA = 0, HBP. = 0, BB. = 0, IBB. = 0, SF. = 0,
                                  HR. = 0, b3. = 0, b2. = 0, b1. = 0, bat. = 0, fld. = 0, bsr. = 0, weight = 3)
    }

    temp.frame  <- rbind(player.data, player.data.2, player.data.3)

    mean.PA <- sum(temp.frame$PA)/nrow(temp.frame)

    temp.frame <- mutate(temp.frame, weight_ratio = PA/mean.PA)

    temp.frame[is.na(temp.frame)] <- 0

    temp.frame <- mutate(temp.frame, weight = weight*weight_ratio)

    temp.frame$HBP. <- temp.frame$HBP.*temp.frame$weight
    temp.frame$IBB. <- temp.frame$IBB.*temp.frame$weight
    temp.frame$BB. <- temp.frame$BB.*temp.frame$weight
    temp.frame$SF. <- temp.frame$SF.*temp.frame$weight
    temp.frame$HR. <- temp.frame$HR.*temp.frame$weight
    temp.frame$b3. <- temp.frame$b3.*temp.frame$weight
    temp.frame$b2. <- temp.frame$b2.*temp.frame$weight
    temp.frame$b1. <- temp.frame$b1.*temp.frame$weight
    temp.frame$bat. <- temp.frame$bat.*temp.frame$weight
    temp.frame$fld. <- temp.frame$fld.*temp.frame$weight
    temp.frame$bsr. <- temp.frame$bsr.*temp.frame$weight

    temp.frame.2 <- data.frame(Name = player, Age = median(temp.frame$Age), PA = sum(temp.frame$PA), HBP. = sum(temp.frame$HBP.)/sum(temp.frame$weight), IBB. = sum(temp.frame$IBB.)/sum(temp.frame$weight),
                               BB. = sum(temp.frame$BB.)/sum(temp.frame$weight), SF. = sum(temp.frame$SF.)/sum(temp.frame$weight), HR. = sum(temp.frame$HR.)/sum(temp.frame$weight),
                               b3. = sum(temp.frame$b3.)/sum(temp.frame$weight), b2. = sum(temp.frame$b2.)/sum(temp.frame$weight), b1. = sum(temp.frame$b1.)/sum(temp.frame$weight),
                               bat. = sum(temp.frame$bat.)/sum(temp.frame$weight), fld. = sum(temp.frame$fld.)/sum(temp.frame$weight), bsr. = sum(temp.frame$bsr.)/sum(temp.frame$weight))

    temp.frame.3 <- data.frame(Name = player, Age = temp.frame.2$Age, HBP. = ((temp.frame.2$HBP.*temp.frame.2$PA) + (lg.averages$HBP.*240))/(sum(temp.frame.2$PA) + 240),
                               BB. = ((temp.frame.2$BB.*temp.frame.2$PA) + (lg.averages$BB.*120))/(sum(temp.frame.2$PA) + 120),
                               IBB. = ((temp.frame.2$IBB.*temp.frame.2$PA) + (lg.averages$IBB.*500))/(sum(temp.frame.2$PA) + 500),
                               SF. = ((temp.frame.2$SF.*temp.frame.2$PA) + (lg.averages$SF.*2000))/(sum(temp.frame.2$PA) + 2000),
                               HR. = ((temp.frame.2$HR.*temp.frame.2$PA) + (lg.averages$HR.*170))/(sum(temp.frame.2$PA) + 170),
                               b3. = ((temp.frame.2$b3.*temp.frame.2$PA) + (lg.averages$b3.*1610))/(sum(temp.frame.2$PA) + 1610),
                               b2. = ((temp.frame.2$b2.*temp.frame.2$PA) + (lg.averages$b2.*1610))/(sum(temp.frame.2$PA) + 1610),
                               b1. = ((temp.frame.2$b1.*temp.frame.2$PA) + (lg.averages$b1.*290))/(sum(temp.frame.2$PA) + 290),
                               bat. = (temp.frame.2$bat.*temp.frame.2$PA)/(sum(temp.frame.2$PA) + 290),
                               fld. = (temp.frame.2$fld.*temp.frame.2$PA)/(sum(temp.frame.2$PA) + 900),
                               bsr. = (temp.frame.2$bsr.*temp.frame.2$PA)/(sum(temp.frame.2$PA) + 400))

    final.frame <- rbind(final.frame, temp.frame.3)

  }

  data("DCHitter2017")
  names(DCHitter2017)[2] <- "Team2017"
  fg.projections <- DCHitter2017
  fg.projections <- fg.projections[,c(1:5)]

  final.frame <- left_join(fg.projections, final.frame, by = c("Name" = "Name"))
  final.frame <- final.frame %>% mutate(HBP = HBP.*PA) %>% mutate(BB = BB.*PA) %>% mutate(IBB = IBB.*PA) %>%
    mutate(SF = SF.*PA) %>% mutate(HR = HR.*PA) %>% mutate(b3 = b3.*PA) %>% mutate(b2 = b2.*PA) %>%
    mutate(b1 = b1.*PA) %>% mutate(bat = bat.*PA) %>% mutate(fld = fld.*PA) %>% mutate(bsr = bsr.*PA)

  wOBA.constants <- wOBA_constants
  wOBA.constants <- filter(wOBA.constants, Season == 2016)

  wBB <- wOBA.constants$wBB
  wHBP <- wOBA.constants$wHBP
  w1B <- wOBA.constants$w1B
  w2B <- wOBA.constants$w2B
  w3B <- wOBA.constants$w3B
  wHR <- wOBA.constants$wHR

  final.frame <- left_join(final.frame, hitter.aging.factors, by = c("Age" = "post.age"))

  final.frame <- final.frame %>% mutate(HBP.delta = hbp.pa.aging*PA) %>% mutate(BB.delta = bb.pa.aging*PA) %>%
    mutate(IBB.delta = ibb.pa.aging*PA) %>% mutate(SF.delta = sf.pa.aging*PA) %>%
    mutate(HR.delta = hr.pa.aging*PA) %>% mutate(b3.delta = b3.pa.aging*PA) %>%
    mutate(b2.delta = b2.pa.aging*PA) %>% mutate(b1.delta = b1.pa.aging*PA) %>%
    mutate(bat.delta = bat.pa.aging*PA) %>% mutate(fld.delta = fld.pa.aging*PA) %>%
    mutate(bsr.delta = bsr.pa.aging*PA)

  final.frame$HBP <- final.frame$HBP + final.frame$HBP.delta
  final.frame$BB <- final.frame$BB + final.frame$BB.delta
  final.frame$IBB <- final.frame$IBB + final.frame$IBB.delta
  final.frame$SF <- final.frame$SF + final.frame$SF.delta
  final.frame$HR <- final.frame$HR + final.frame$HR.delta
  final.frame$b3 <- final.frame$b3 + final.frame$b3.delta
  final.frame$b2 <- final.frame$b2 + final.frame$b2.delta
  final.frame$b1 <- final.frame$b1 + final.frame$b1.delta
  final.frame$fld <- final.frame$fld + final.frame$fld.delta
  final.frame$bat <- final.frame$bat + final.frame$bat.delta
  final.frame$bsr <- final.frame$bsr + final.frame$bsr.delta

  final.frame <- final.frame %>%
    mutate(pawOBA = (wBB*BB + wHBP*HBP + w1B*b1 + w2B*b2 + w3B*b3 + wHR*HR)/(AB + BB - IBB + SF + HBP))

  for (i in 1:length(final.frame$Team2017)){
    if (final.frame$Team2017[i] == ""){
      final.frame$Team2017[i] = "N/A"
    }
  }

  final.frame <- final.frame[complete.cases(final.frame),]

  final.frame <- left_join(final.frame, pf.2016, by = c("Team2017" = "Team"))
  names(final.frame)[19] <- "BB"
  names(final.frame)[22] <- "HR"
  final.frame[is.na(final.frame)] <- 1
  final.frame <- final.frame %>% mutate(BB = BB*BB.y) %>% mutate(HR = HR*HR.y) %>%
    mutate(b3 = b3*`3B`) %>% mutate(b2 = b2*`2B`) %>% mutate(b1 = b1*`1B`)

  final.frame <- final.frame %>%
    mutate(wOBA = (wBB*BB + wHBP*HBP + w1B*b1 + w2B*b2 + w3B*b3 + wHR*HR)/(AB + BB - IBB + SF + HBP))

  final.frame <- final.frame[,c(1:52,66)]

  fielding.data <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=fld&lg=all&qual=0&type=1&season=2016&month=0&season1=2014&ind=1&team=0&rost=0&age=0&filter=&players=0&page=1_7000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00[,2:6]
  for (i in c(1,5)){
    class(fielding.data[,i]) <- "numeric"
  }

  fielding.data <- fielding.data[order(-fielding.data$Season, -fielding.data$Inn),]
  fielding.data <- fielding.data[!(duplicated(fielding.data$Name)),]

  fielding.data <- fielding.data[,c(2:4)]

  final.frame <- left_join(final.frame, fielding.data)

  rf.pos.pa <- (-4.25)/680
  lf.pos.pa <- (-4.25)/680
  cf.pos.pa <- (1.75)/680
  b3.pos.pa <- (1.75)/680
  b2.pos.pa <- (1.75)/680
  ss.pos.pa <- (4.75)/680
  c.pos.pa <- (7.75)/680
  b1.pos.pa <- (-9.25)/680
  dh.pos.pa <- (-9.25)/680

  final.frame <- final.frame %>% mutate(pos. = ifelse(Pos == "1B", b1.pos.pa, 0))
  final.frame <- final.frame %>% mutate(pos. = ifelse(Pos == "C", c.pos.pa, pos.)) %>% mutate(pos. = ifelse(Pos == "SS", ss.pos.pa, pos.)) %>%
    mutate(pos. = ifelse(Pos == "2B", b2.pos.pa, pos.)) %>% mutate(pos. = ifelse(Pos == "3B", b3.pos.pa, pos.)) %>%
    mutate(pos. = ifelse(Pos == "CF", cf.pos.pa, pos.)) %>% mutate(pos. = ifelse(Pos == "LF", lf.pos.pa, pos.)) %>%
    mutate(pos. = ifelse(Pos == "RF", rf.pos.pa, pos.))

  final.frame <- mutate(final.frame, Pos = pos.*PA)

  al.lg.adj.frame <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=al&qual=0&type=c,6,53,111,54,56&season=2016&month=0&season1=2016&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_800", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  nl.lg.adj.frame <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=nl&qual=0&type=c,6,53,111,54,56&season=2016&month=0&season1=2016&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_800", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00

  for(i in c(1,4:8)){
    class(al.lg.adj.frame[,i]) <- "numeric"
    class(nl.lg.adj.frame[,i]) <- "numeric"
  }

  al.lg.adj.frame[is.na(al.lg.adj.frame)] <- 0
  nl.lg.adj.frame[is.na(nl.lg.adj.frame)] <- 0

  al.lg.adj <- ((-1)*((sum(al.lg.adj.frame$Bat) + sum(al.lg.adj.frame$BsR) + sum(al.lg.adj.frame$Fld) +
    sum(al.lg.adj.frame$Pos))/sum(al.lg.adj.frame$PA)))

  nl.lg.adj <- ((-1)*((sum(nl.lg.adj.frame$Bat) + sum(nl.lg.adj.frame$BsR) + sum(nl.lg.adj.frame$Fld) +
                         sum(nl.lg.adj.frame$Pos))/sum(nl.lg.adj.frame$PA)))

  mix.lg.adj <- ((al.lg.adj+nl.lg.adj)/2) - nl.lg.adj

  al.vect <- c("BAL", "NYY", "TOR", "TBR", "BOS", "DET", "CLE", "CHW", "KCR", "MIN", "SEA", "LAA", "OAK", "TEX", "HOU")

  final.frame <- final.frame %>% mutate(AL = ifelse(Team %in% al.vect, 1, 0))
  final.frame <- mutate(final.frame, mix.lg = ifelse(Team == "- - -", 1, 0))

  final.frame <- final.frame %>% mutate(lg.adj = ifelse(AL == 1, al.lg.adj, nl.lg.adj))
  final.frame <- final.frame %>% mutate(lg.adj = ifelse(mix.lg == 1, lg.adj + mix.lg.adj, lg.adj))

  final.frame$lg.adj <- final.frame$lg.adj*final.frame$PA

  lg.avg.wOBA <- wOBA.constants$wOBA
  wOBA.scale <- wOBA.constants$wOBAScale

  final.frame <- mutate(final.frame, wRAA = ((wOBA - lg.avg.wOBA)/wOBA.scale)*PA)

  fg.park.factors <- pf.2016[,c(2:3)]

  final.frame$Team <- NULL
  names(final.frame)[2] <- "Team"

  final.frame <- left_join(final.frame, fg.park.factors)
  names(final.frame)[60] <- "fg.pf"
  final.frame <- mutate(final.frame, fg.pf = ifelse(Team == "- - -", 1, fg.pf))

  al.wrc.frame <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=al&qual=0&type=c,6,52&season=2016&month=0&season1=2016&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_400", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  nl.wrc.frame <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=nl&qual=0&type=c,6,52&season=2016&month=0&season1=2016&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_400", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  for (i in c(1,4:5)){
    class(al.wrc.frame[,i]) <- "numeric"
    class(nl.wrc.frame[,i]) <- "numeric"
  }

  al.wrc.pa <- sum(al.wrc.frame$wRC)/sum(al.wrc.frame$PA)
  nl.wrc.pa <- sum(nl.wrc.frame$wRC)/sum(nl.wrc.frame$PA)
  mix.wrc.pa <- (al.wrc.pa + nl.wrc.pa)/2

  lg.run.frame <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,12&season=2016&month=0&season1=2016&ind=0&team=&rost=&age=&filter=&players=&page=1_1100", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  for(i in c(1,4:5)){
    class(lg.run.frame[,i]) <- "numeric"
  }
  lg.r.pa <- sum(lg.run.frame$R)/sum(lg.run.frame$PA)
  lg.pa <- sum(lg.run.frame$PA)

  final.frame <- mutate(final.frame, wrc.pa = ifelse(AL == 1, al.wrc.pa, nl.wrc.pa))
  final.frame <- mutate(final.frame, wrc.pa = ifelse(mix.lg == 1, mix.wrc.pa, wrc.pa))
  final.frame <- mutate(final.frame, r.pa = lg.r.pa)

  final.frame <- mutate(final.frame, batting.runs = wRAA + (r.pa - (fg.pf*r.pa))*PA + (r.pa - wrc.pa)*PA)

  final.frame <- mutate(final.frame, replacement.runs = 570*(wOBA.constants$R.W/lg.pa)*PA)

  final.frame <- mutate(final.frame, WAR = (batting.runs + bsr + fld + Pos + lg.adj + replacement.runs)/wOBA.constants$R.W)

  final.frame <- mutate(final.frame, WAR.600 = (WAR/PA)*600)

  final.frame <- final.frame[complete.cases(final.frame),]
  final.frame <- final.frame[!duplicated(final.frame),]

  final.frame <- filter(final.frame, Name != "Matt Duffy" & Name != "Chris Young")

  final.frame <- final.frame %>% mutate(OBP = (b1 + b2 + b3 + HR + BB + IBB + HBP)/(AB + BB + SF)) %>%
    mutate(AVG = (b1 + b2 + b3 + HR)/AB) %>% mutate(SLG = (b1 + 2*b2 + 3*b3 + 4*HR)/AB)

  return(final.frame)

}
