
volatility_projections <- function(projection.year, hitter.aging.factors){

  library(dplyr)
  library(XML)

  third.season <- projection.year - 3
  second.season <- projection.year - 2
  past.season <- projection.year - 1

  raw.data <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,5,6,8,9,10,11,14,15,17,18,50,3,53,54,111&season=", past.season, "&month=0&season1=", third.season, "&ind=1&team=0&rost=0&age=0&filter=&players=0&sort=2,d&page=1_5000"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  lg.avg.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,5,6,8,9,10,11,14,15,17,18,50,3,53,54,111&season=", past.season, "&month=0&season1=", past.season, "&ind=1&team=0&rost=0&age=0&filter=&players=0&page=1_2000"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

  raw.data[is.na(raw.data)] <- 0

  names(raw.data)[7:9] <- c("b1", "b2", "b3")

  past.statistics <- raw.data %>% mutate(b1. = b1/PA) %>% mutate(b2. = b2/PA) %>% mutate(b3. = b3/PA) %>%
    mutate(HR. = HR/PA) %>% mutate(BB. = BB/PA) %>% mutate(IBB. = IBB/PA) %>% mutate(HBP. = HBP/PA) %>%
    mutate(SF. = SF/PA) %>% mutate(fld. = Fld/PA) %>% mutate(bsr. = BsR/PA)

  past.statistics[is.na(past.statistics)] <- 0

  stats.past.season <- filter(past.statistics, Season == past.season)
  stats.second.season <- filter(past.statistics, Season == second.season)
  stats.third.season <- filter(past.statistics, Season == third.season)

  stats.past.season$Age <- stats.past.season$Age + 1
  stats.second.season$Age <- stats.second.season$Age + 2
  stats.third.season$Age <- stats.third.season$Age + 3

  lg.averages <- data.frame(HBP. = sum(lg.avg.frame$HBP)/sum(lg.avg.frame$PA),
                            BB. = sum(lg.avg.frame$BB)/sum(lg.avg.frame$PA),
                            IBB. = sum(lg.avg.frame$IBB)/sum(lg.avg.frame$PA),
                            SF. = sum(lg.avg.frame$SF)/sum(lg.avg.frame$PA),
                            HR. = sum(lg.avg.frame$HR)/sum(lg.avg.frame$PA),
                            b3. = sum(lg.avg.frame$`3B`)/sum(lg.avg.frame$PA),
                            b2. = sum(lg.avg.frame$`2B`)/sum(lg.avg.frame$PA),
                            b1. = sum(lg.avg.frame$`1B`)/sum(lg.avg.frame$PA),
                            bat. = 0, fld. = 0, bsr. = 0)

  age.frame <- unique(rbind(stats.past.season, stats.second.season, stats.third.season)[,c(3,16)])

  final.frame <- data.frame(Name = character(), Age = numeric(), HBP. = numeric(), BB. = numeric(),
                            IBB. = numeric(), SF. = numeric(), HR. = numeric(), b3. = numeric(),
                            b2. = numeric(), b1. = numeric(), fld. = numeric(),
                            bsr. = numeric(), pa.past.year = numeric(), pa.second.season = numeric(),
                            pa.third.season = numeric())

  for(i in 1:nrow(age.frame)){

    player <- age.frame$Name[i]
    player.age <- age.frame$Age[i]

    if(player %in% stats.past.season$Name){
      index <- match(player, stats.past.season$Name)
      temp.data <- stats.past.season[index,]
      player.data <- temp.data[,c(2:3,16,6,26,24:25,27,23,22,21,20,28,29)]
      player.data$weight <- 5
    } else{
      player.data <- data.frame(Season = past.season, Name = player, Age = player.age,
                                PA = 0, HBP. = 0, BB. = 0, IBB. = 0, SF. = 0,
                                HR. = 0, b3. = 0, b2. = 0, b1. = 0, fld. = 0, bsr. = 0, weight = 5)
    }

    if(player %in% stats.second.season$Name){
      index <- match(player, stats.second.season$Name)
      temp.data <- stats.second.season[index,]
      player.data.2 <- temp.data[,c(2:3,16,6,26,24:25,27,23,22,21,20,28,29)]
      player.data.2$weight <- 4
    } else{
      player.data.2 <- data.frame(Season = second.season, Name = player, Age = player.age,
                                  PA = 0, HBP. = 0, BB. = 0, IBB. = 0, SF. = 0,
                                  HR. = 0, b3. = 0, b2. = 0, b1. = 0, fld. = 0, bsr. = 0, weight = 4)
    }

    if(player %in% stats.third.season$Name){
      index <- match(player, stats.third.season$Name)
      temp.data <- stats.third.season[index,]
      player.data.3 <- temp.data[,c(2:3,16,6,26,24:25,27,23,22,21,20,28,29)]
      player.data.3$weight <- 3
    } else{
      player.data.3 <- data.frame(Season = third.season, Name = player, Age = player.age,
                                  PA = 0, HBP. = 0, BB. = 0, IBB. = 0, SF. = 0,
                                  HR. = 0, b3. = 0, b2. = 0, b1. = 0, fld. = 0, bsr. = 0, weight = 3)
    }

    pa.past.year <- player.data$PA
    pa.second.season <- player.data.2$PA
    pa.third.season <- player.data.3$PA

    temp.frame  <- rbind(player.data, player.data.2, player.data.3)

    mean.PA <- sum(temp.frame$PA)/nrow(temp.frame)

    temp.frame <- mutate(temp.frame, weight_ratio = PA/mean.PA)

    temp.frame[is.na(temp.frame)] <- 0

    temp.frame <- mutate(temp.frame, weight = weight*weight_ratio)

    #     for(j in 1:nrow(temp.frame)){
    #       if (temp.frame$PA[j] < 120){
    #         temp.frame$BB.[j] <- ((temp.frame$BB.[j]*temp.frame$PA[j]) + lg.averages$BB.*(120 - temp.frame$PA[j]))/120
    #         temp.frame$HBP.[j] <- ((temp.frame$HBP.[j]*temp.frame$PA[j]) + lg.averages$HBP.*(240 - temp.frame$PA[j]))/240
    #         temp.frame$IBB.[j] <- ((temp.frame$IBB.[j]*temp.frame$PA[j]) + lg.averages$IBB.*(500 - temp.frame$PA[j]))/500
    #         temp.frame$SF.[j] <- ((temp.frame$SF.[j]*temp.frame$PA[j]) + lg.averages$SF.*(2000 - temp.frame$PA[j]))/2000
    #         temp.frame$HR.[j] <- ((temp.frame$HR.[j]*temp.frame$PA[j]) + lg.averages$HR.*(170 - temp.frame$PA[j]))/170
    #         temp.frame$b3.[j] <- ((temp.frame$b3.[j]*temp.frame$PA[j]) + lg.averages$b3.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b2.[j] <- ((temp.frame$b2.[j]*temp.frame$PA[j]) + lg.averages$b2.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b1.[j] <- ((temp.frame$b1.[j]*temp.frame$PA[j]) + lg.averages$b1.*(290 - temp.frame$PA[j]))/290
    #       }
    #       else if(temp.frame$PA[j] < 170 & temp.frame$PA[j] >= 120){
    #         temp.frame$HBP.[j] <- ((temp.frame$HBP.[j]*temp.frame$PA[j]) + lg.averages$HBP.*(240 - temp.frame$PA[j]))/240
    #         temp.frame$IBB.[j] <- ((temp.frame$IBB.[j]*temp.frame$PA[j]) + lg.averages$IBB.*(500 - temp.frame$PA[j]))/500
    #         temp.frame$SF.[j] <- ((temp.frame$SF.[j]*temp.frame$PA[j]) + lg.averages$SF.*(2000 - temp.frame$PA[j]))/2000
    #         temp.frame$HR.[j] <- ((temp.frame$HR.[j]*temp.frame$PA[j]) + lg.averages$HR.*(170 - temp.frame$PA[j]))/170
    #         temp.frame$b3.[j] <- ((temp.frame$b3.[j]*temp.frame$PA[j]) + lg.averages$b3.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b2.[j] <- ((temp.frame$b2.[j]*temp.frame$PA[j]) + lg.averages$b2.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b1.[j] <- ((temp.frame$b1.[j]*temp.frame$PA[j]) + lg.averages$b1.*(290 - temp.frame$PA[j]))/290
    #       }
    #       else if (temp.frame$PA[j] < 240 & temp.frame$PA[j] >= 170){
    #         temp.frame$HBP.[j] <- ((temp.frame$HBP.[j]*temp.frame$PA[j]) + lg.averages$HBP.*(240 - temp.frame$PA[j]))/240
    #         temp.frame$IBB.[j] <- ((temp.frame$IBB.[j]*temp.frame$PA[j]) + lg.averages$IBB.*(500 - temp.frame$PA[j]))/500
    #         temp.frame$SF.[j] <- ((temp.frame$SF.[j]*temp.frame$PA[j]) + lg.averages$SF.*(2000 - temp.frame$PA[j]))/2000
    #         temp.frame$b3.[j] <- ((temp.frame$b3.[j]*temp.frame$PA[j]) + lg.averages$b3.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b2.[j] <- ((temp.frame$b2.[j]*temp.frame$PA[j]) + lg.averages$b2.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b1.[j] <- ((temp.frame$b1.[j]*temp.frame$PA[j]) + lg.averages$b1.*(290 - temp.frame$PA[j]))/290
    #       }
    #       else if (temp.frame$PA[j] < 290 & temp.frame$PA[j] >= 240){
    #         temp.frame$IBB.[j] <- ((temp.frame$IBB.[j]*temp.frame$PA[j]) + lg.averages$IBB.*(500 - temp.frame$PA[j]))/500
    #         temp.frame$SF.[j] <- ((temp.frame$SF.[j]*temp.frame$PA[j]) + lg.averages$SF.*(2000 - temp.frame$PA[j]))/2000
    #         temp.frame$b3.[j] <- ((temp.frame$b3.[j]*temp.frame$PA[j]) + lg.averages$b3.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b2.[j] <- ((temp.frame$b2.[j]*temp.frame$PA[j]) + lg.averages$b2.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b1.[j] <- ((temp.frame$b1.[j]*temp.frame$PA[j]) + lg.averages$b1.*(290 - temp.frame$PA[j]))/290
    #       }
    #       else if (temp.frame$PA[j] < 500 & temp.frame$PA[j] >= 290){
    #         temp.frame$IBB.[j] <- ((temp.frame$IBB.[j]*temp.frame$PA[j]) + lg.averages$IBB.*(500 - temp.frame$PA[j]))/500
    #         temp.frame$SF.[j] <- ((temp.frame$SF.[j]*temp.frame$PA[j]) + lg.averages$SF.*(2000 - temp.frame$PA[j]))/2000
    #         temp.frame$b3.[j] <- ((temp.frame$b3.[j]*temp.frame$PA[j]) + lg.averages$b3.*(1610 - temp.frame$PA[j]))/1610
    #         temp.frame$b2.[j] <- ((temp.frame$b2.[j]*temp.frame$PA[j]) + lg.averages$b2.*(1610 - temp.frame$PA[j]))/1610
    #       }
    #     }


    temp.frame$HBP. <- temp.frame$HBP.*temp.frame$weight
    temp.frame$IBB. <- temp.frame$IBB.*temp.frame$weight
    temp.frame$BB. <- temp.frame$BB.*temp.frame$weight
    temp.frame$SF. <- temp.frame$SF.*temp.frame$weight
    temp.frame$HR. <- temp.frame$HR.*temp.frame$weight
    temp.frame$b3. <- temp.frame$b3.*temp.frame$weight
    temp.frame$b2. <- temp.frame$b2.*temp.frame$weight
    temp.frame$b1. <- temp.frame$b1.*temp.frame$weight
    temp.frame$fld. <- temp.frame$fld.*temp.frame$weight
    temp.frame$bsr. <- temp.frame$bsr.*temp.frame$weight

    temp.frame.2 <- data.frame(Name = player, Age = median(temp.frame$Age), PA = sum(temp.frame$PA), HBP. = sum(temp.frame$HBP.)/sum(temp.frame$weight), IBB. = sum(temp.frame$IBB.)/sum(temp.frame$weight),
                               BB. = sum(temp.frame$BB.)/sum(temp.frame$weight), SF. = sum(temp.frame$SF.)/sum(temp.frame$weight), HR. = sum(temp.frame$HR.)/sum(temp.frame$weight),
                               b3. = sum(temp.frame$b3.)/sum(temp.frame$weight), b2. = sum(temp.frame$b2.)/sum(temp.frame$weight), b1. = sum(temp.frame$b1.)/sum(temp.frame$weight),
                               fld. = sum(temp.frame$fld.)/sum(temp.frame$weight), bsr. = sum(temp.frame$bsr.)/sum(temp.frame$weight))

    temp.frame.3 <- data.frame(Name = player, Age = temp.frame.2$Age, HBP. = ((temp.frame.2$HBP.*temp.frame.2$PA) + (lg.averages$HBP.*240))/(sum(temp.frame.2$PA) + 240),
                               BB. = ((temp.frame.2$BB.*temp.frame.2$PA) + (lg.averages$BB.*120))/(sum(temp.frame.2$PA) + 120),
                               IBB. = ((temp.frame.2$IBB.*temp.frame.2$PA) + (lg.averages$IBB.*500))/(sum(temp.frame.2$PA) + 500),
                               SF. = ((temp.frame.2$SF.*temp.frame.2$PA) + (lg.averages$SF.*2000))/(sum(temp.frame.2$PA) + 2000),
                               HR. = ((temp.frame.2$HR.*temp.frame.2$PA) + (lg.averages$HR.*170))/(sum(temp.frame.2$PA) + 170),
                               b3. = ((temp.frame.2$b3.*temp.frame.2$PA) + (lg.averages$b3.*1610))/(sum(temp.frame.2$PA) + 1610),
                               b2. = ((temp.frame.2$b2.*temp.frame.2$PA) + (lg.averages$b2.*1610))/(sum(temp.frame.2$PA) + 1610),
                               b1. = ((temp.frame.2$b1.*temp.frame.2$PA) + (lg.averages$b1.*290))/(sum(temp.frame.2$PA) + 290),
                               fld. = (temp.frame.2$fld.*temp.frame.2$PA)/(sum(temp.frame.2$PA) + 1200),
                               bsr. = (temp.frame.2$bsr.*temp.frame.2$PA)/(sum(temp.frame.2$PA) + 400),
                               pa.past.year = pa.past.year, pa.second.season = pa.second.season,
                               pa.third.season = pa.third.season)

    final.frame <- rbind(final.frame, temp.frame.3)

  }

  final.frame <- final.frame[complete.cases(final.frame),]

  fg.final.stats <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,5,6,8,9,10,11,14,15,17,18,50,3,53,54,111&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_200"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

  fg.final.stats <- fg.final.stats[,c(2,4:5)]

  final.frame <- left_join(fg.final.stats, final.frame, by = c("Name" = "Name"))

  final.frame <- final.frame %>% mutate(HBP = HBP.*PA) %>% mutate(BB = BB.*PA) %>% mutate(IBB = IBB.*PA) %>%
    mutate(SF = SF.*PA) %>% mutate(HR = HR.*PA) %>% mutate(b3 = b3.*PA) %>% mutate(b2 = b2.*PA) %>%
    mutate(b1 = b1.*PA) %>% mutate(fld = fld.*PA) %>% mutate(bsr = bsr.*PA)

  wOBA.constants <- readHTMLTable("http://www.fangraphs.com/guts.aspx?type=cn", which = 16, stringsAsFactors = FALSE, colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  wOBA.constants <- filter(wOBA.constants, Season == past.season)

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
    mutate(fld.delta = fld.pa.aging*PA) %>% mutate(bsr.delta = bsr.pa.aging*PA)

  final.frame$HBP <- final.frame$HBP + final.frame$HBP.delta
  final.frame$BB <- final.frame$BB + final.frame$BB.delta
  final.frame$IBB <- final.frame$IBB + final.frame$IBB.delta
  final.frame$SF <- final.frame$SF + final.frame$SF.delta
  final.frame$HR <- final.frame$HR + final.frame$HR.delta
  final.frame$b3 <- final.frame$b3 + final.frame$b3.delta
  final.frame$b2 <- final.frame$b2 + final.frame$b2.delta
  final.frame$b1 <- final.frame$b1 + final.frame$b1.delta
  final.frame$fld <- final.frame$fld + final.frame$fld.delta
  final.frame$bsr <- final.frame$bsr + final.frame$bsr.delta


  final.frame <- final.frame %>%
    mutate(wOBA = (wBB*BB + wHBP*HBP + w1B*b1 + w2B*b2 + w3B*b3 + wHR*HR)/(AB + BB - IBB + SF + HBP))

  final.frame <- final.frame[complete.cases(final.frame),]

  fielding.data <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=fld&lg=all&qual=0&type=1&season=", past.season, "&month=0&season1=", third.season, "&ind=1&team=0&rost=0&age=0&filter=&players=0&page=1_7000"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "numeric", "character", "character", "character", "numeric"))[,2:6]
  fielding.data <- fielding.data[order(-fielding.data$Season, -fielding.data$Inn),]
  fielding.data <- fielding.data[!(duplicated(fielding.data$Name)),]

  fielding.data <- fielding.data[,c(2:4)]

  final.frame <- left_join(final.frame, fielding.data)

  positional.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,3,6,56&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_1500"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric"))
  positional.frame <- mutate(positional.frame, pos. = Pos/PA)
  positional.frame <- positional.frame[,c(2,7)]

  final.frame <- left_join(final.frame, positional.frame)
  final.frame <- mutate(final.frame, Pos = pos.*PA)

  al.lg.adj.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=al&qual=0&type=c,6,53,111,54,56&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_800"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric"))
  nl.lg.adj.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=nl&qual=0&type=c,6,53,111,54,56&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_800"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric"))

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

  final.frame$Team <- NULL

  fg.park.factors <- readHTMLTable(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", past.season), which = 16, colClasses = c("numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

  fg.park.factors <- fg.park.factors[,c(2:3)]

  team.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6&season=", past.season, "&month=0&season1=", third.season, "&ind=1&team=&rost=&age=&filter=&players=&page=1_5000"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "numeric", "character", "character", "numeric"))[2:4]
  team.frame <- team.frame[order(-team.frame$Season),]
  team.frame <- team.frame[!(duplicated(team.frame$Name)),]
  team.frame <- team.frame[,2:3]

  final.frame <- left_join(final.frame, team.frame)

  final.frame <- left_join(final.frame, fg.park.factors)
  names(final.frame)[58] <- "fg.pf"
  final.frame <- mutate(final.frame, fg.pf = ifelse(Team == "- - -", 100, fg.pf))
  final.frame$fg.pf <- final.frame$fg.pf/100

  al.wrc.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=al&qual=0&type=c,6,52&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_400"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric"))
  nl.wrc.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=nl&qual=0&type=c,6,52&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_400"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric"))

  al.wrc.pa <- sum(al.wrc.frame$wRC)/sum(al.wrc.frame$PA)
  nl.wrc.pa <- sum(nl.wrc.frame$wRC)/sum(nl.wrc.frame$PA)
  mix.wrc.pa <- (al.wrc.pa + nl.wrc.pa)/2

  lg.run.frame <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,12&season=", past.season, "&month=0&season1=", past.season, "&ind=0&team=&rost=&age=&filter=&players=&page=1_1100"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric"))
  lg.r.pa <- sum(lg.run.frame$R)/sum(lg.run.frame$PA)
  lg.pa <- sum(lg.run.frame$PA)

  final.frame <- mutate(final.frame, wrc.pa = ifelse(AL == 1, al.wrc.pa, nl.wrc.pa))
  final.frame <- mutate(final.frame, wrc.pa = ifelse(mix.lg == 1, mix.wrc.pa, wrc.pa))
  final.frame <- mutate(final.frame, r.pa = lg.r.pa)

  final.frame <- mutate(final.frame, batting.runs = wRAA + (r.pa - (fg.pf*r.pa))*PA + (r.pa - wrc.pa)*PA)

  final.frame <- mutate(final.frame, replacement.runs = 570*(wOBA.constants$`R/W`/lg.pa)*PA)

  final.frame <- mutate(final.frame, WAR = (batting.runs + bsr + fld + Pos + lg.adj + replacement.runs)/wOBA.constants$`R/W`)

  final.frame <- mutate(final.frame, WAR.600 = (WAR/PA)*600)

  final.frame <- final.frame[complete.cases(final.frame),]
  final.frame <- final.frame[!duplicated(final.frame),]

  actual.war <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,6,58,50,54,111&season=", projection.year, "&month=0&season1=", projection.year,"&ind=0&team=&rost=&age=&filter=&players=&page=1_200"), which = 23, stringsAsFactors = FALSE, colClasses = c("numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric"))
  actual.war <- mutate(actual.war, a.WAR.600 = (WAR/PA)*600)
  actual.war <- actual.war %>% mutate(a.fld.600 = (Fld/PA)*600) %>% mutate(a.bsr.600 = (BsR/PA)*600)
  names(actual.war)[5] <- "a.WAR"
  names(actual.war)[6] <- "a.wOBA"
  names(actual.war)[7] <- "a.Fld"
  names(actual.war)[8] <- "a.BsR"
  actual.war <- actual.war[,c(2,5:11)]

  return.frame <- left_join(actual.war, final.frame, by = c("Name" = "Name"))

  return(return.frame)

}

