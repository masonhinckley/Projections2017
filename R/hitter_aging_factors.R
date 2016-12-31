

hitter_aging_factors <- function(){

  library(dplyr)
  library(XML)

  raw.data <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,5,6,8,9,10,11,14,15,17,18,50,3,53,54,111&season=2014&month=0&season1=2003&ind=1&team=0&rost=0&age=0&filter=&players=0&page=1_2000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  for (i in c(1,2,5:19)){
    class(raw.data[,i]) <- "numeric"
  }

  names(raw.data)[7:9] <- c("b1", "b2", "b3")

  raw.data <- raw.data %>% mutate(b1.pa = b1/PA) %>% mutate(b2.pa = b2/PA) %>% mutate(b3.pa = b3/PA) %>%
    mutate(hr.pa = HR/PA) %>% mutate(bb.pa = BB/PA) %>% mutate(ibb.pa = IBB/PA) %>% mutate(hbp.pa = HBP/PA) %>%
    mutate(sf.pa = SF/PA) %>% mutate(bat.pa = Bat/PA) %>% mutate(fld.pa = Fld/PA) %>%
    mutate(bsr.pa = BsR/PA)

  raw.data <- raw.data[complete.cases(raw.data),]

  temp.frame <- data.frame(NULL)

  min.age <- min(raw.data$Age)
  max.age <- max(raw.data$Age)

  for (i in min.age:(max.age-1)){

    pre.age <- i
    post.age <- i + 1

    pre.frame <- filter(raw.data, Age == pre.age)
    post.frame <- filter(raw.data, Age == post.age)

    pre.frame <- filter(pre.frame, Name %in% post.frame$Name)
    post.frame <- filter(post.frame, Name %in% pre.frame$Name)

    age.frame <- data.frame(NULL)

    if(nrow(post.frame) >= 5){

      for (i in 1:nrow(pre.frame)){

        temp.player <- pre.frame$Name[i]
        pre.player <- filter(pre.frame, Name == temp.player)
        post.player <- filter(post.frame, Name == temp.player)

        age.frame <- rbind(age.frame, data.frame(Name = temp.player, wOBA.diff = post.player$wOBA - pre.player$wOBA,
                                                 b1.pa.diff = post.player$b1.pa - pre.player$b1.pa,
                                                 b2.pa.diff = post.player$b2.pa - pre.player$b2.pa,
                                                 b3.pa.diff = post.player$b3.pa - pre.player$b3.pa,
                                                 hr.pa.diff = post.player$hr.pa - pre.player$hr.pa,
                                                 bb.pa.diff = post.player$bb.pa - pre.player$bb.pa,
                                                 ibb.pa.diff = post.player$ibb.pa - pre.player$ibb.pa,
                                                 hbp.pa.diff = post.player$hbp.pa - pre.player$hbp.pa,
                                                 sf.pa.diff = post.player$sf.pa - pre.player$sf.pa,
                                                 bat.pa.diff = post.player$bat.pa - pre.player$bat.pa,
                                                 fld.pa.diff = post.player$fld.pa - pre.player$fld.pa,
                                                 bsr.pa.diff = post.player$bsr.pa - pre.player$bsr.pa,
                                                 weight = 2/((1/pre.player$PA) + (1/post.player$PA))))

      }

      age.frame$wOBA.diff <- age.frame$wOBA.diff*age.frame$weight
      age.frame$b1.pa.diff <- age.frame$b1.pa.diff*age.frame$weight
      age.frame$b2.pa.diff <- age.frame$b2.pa.diff*age.frame$weight
      age.frame$b3.pa.diff <- age.frame$b3.pa.diff*age.frame$weight
      age.frame$hr.pa.diff <- age.frame$hr.pa.diff*age.frame$weight
      age.frame$bb.pa.diff <- age.frame$bb.pa.diff*age.frame$weight
      age.frame$ibb.pa.diff <- age.frame$ibb.pa.diff*age.frame$weight
      age.frame$hbp.pa.diff <- age.frame$hbp.pa.diff*age.frame$weight
      age.frame$sf.pa.diff <- age.frame$sf.pa.diff*age.frame$weight
      age.frame$bat.pa.diff <- age.frame$bat.pa.diff*age.frame$weight
      age.frame$fld.pa.diff <- age.frame$fld.pa.diff*age.frame$weight
      age.frame$bsr.pa.diff <- age.frame$bsr.pa.diff*age.frame$weight

      temp.frame <- rbind(temp.frame, data.frame(post.age = post.age, wOBA.aging = sum(age.frame$wOBA.diff)/sum(age.frame$weight),
                                                 b1.pa.aging = sum(age.frame$b1.pa.diff)/sum(age.frame$weight),
                                                 b2.pa.aging = sum(age.frame$b2.pa.diff)/sum(age.frame$weight),
                                                 b3.pa.aging = sum(age.frame$b3.pa.diff)/sum(age.frame$weight),
                                                 hr.pa.aging = sum(age.frame$hr.pa.diff)/sum(age.frame$weight),
                                                 bb.pa.aging = sum(age.frame$bb.pa.diff)/sum(age.frame$weight),
                                                 ibb.pa.aging = sum(age.frame$ibb.pa.diff)/sum(age.frame$weight),
                                                 hbp.pa.aging = sum(age.frame$hbp.pa.diff)/sum(age.frame$weight),
                                                 sf.pa.aging = sum(age.frame$sf.pa.diff)/sum(age.frame$weight),
                                                 bat.pa.aging = sum(age.frame$bat.pa.diff)/sum(age.frame$weight),
                                                 fld.pa.aging = sum(age.frame$fld.pa.diff)/sum(age.frame$weight),
                                                 bsr.pa.aging = sum(age.frame$bsr.pa.diff)/sum(age.frame$weight)))
    }

  }

  eval.num <- ((max(temp.frame$post.age) - min(temp.frame$post.age))*2) + 1

  lw.wOBA <- loess.smooth(temp.frame$post.age, temp.frame$wOBA.aging, evaluation = eval.num)
  lw.b1 <- loess.smooth(temp.frame$post.age, temp.frame$b1.pa.aging, evaluation = eval.num)
  lw.b2 <- loess.smooth(temp.frame$post.age, temp.frame$b2.pa.aging, evaluation = eval.num)
  lw.b3 <- loess.smooth(temp.frame$post.age, temp.frame$b3.pa.aging, evaluation = eval.num)
  lw.hr <- loess.smooth(temp.frame$post.age, temp.frame$hr.pa.aging, evaluation = eval.num)
  lw.bb <- loess.smooth(temp.frame$post.age, temp.frame$bb.pa.aging, evaluation = eval.num)
  lw.ibb <- loess.smooth(temp.frame$post.age, temp.frame$ibb.pa.aging, evaluation = eval.num)
  lw.hbp <- loess.smooth(temp.frame$post.age, temp.frame$hbp.pa.aging, evaluation = eval.num)
  lw.sf <- loess.smooth(temp.frame$post.age, temp.frame$sf.pa.aging, evaluation = eval.num)
  lw.bat <- loess.smooth(temp.frame$post.age, temp.frame$bat.pa.aging, evaluation = eval.num)
  lw.fld <- loess.smooth(temp.frame$post.age, temp.frame$fld.pa.aging, evaluation = eval.num)
  lw.bsr <- loess.smooth(temp.frame$post.age, temp.frame$bsr.pa.aging, evaluation = eval.num)

  final.frame <- data.frame(post.age = lw.wOBA[[1]][seq(1,eval.num,2)], wOBA.aging = lw.wOBA[[2]][seq(1,eval.num,2)],
                            b1.pa.aging = lw.b1[[2]][seq(1,eval.num,2)], b2.pa.aging = lw.b2[[2]][seq(1,eval.num,2)],
                            b3.pa.aging = lw.b3[[2]][seq(1,eval.num,2)], hr.pa.aging = lw.hr[[2]][seq(1,eval.num,2)],
                            bb.pa.aging = lw.bb[[2]][seq(1,eval.num,2)], ibb.pa.aging = lw.ibb[[2]][seq(1,eval.num,2)],
                            hbp.pa.aging = lw.hbp[[2]][seq(1,eval.num,2)], sf.pa.aging = lw.sf[[2]][seq(1,eval.num,2)],
                            bat.pa.aging = lw.bat[[2]][seq(1,eval.num,2)], fld.pa.aging = lw.fld[[2]][seq(1,eval.num,2)],
                            bsr.pa.aging = lw.bsr[[2]][seq(1,eval.num,2)])

  for (i in 40:60){
    if (!(i %in% final.frame$post.age)){
      final.frame <- rbind(final.frame, data.frame(post.age = i, wOBA.aging = final.frame[18,2], b1.pa.aging = final.frame[18,3], b2.pa.aging = final.frame[18,4],
                                                   b3.pa.aging = final.frame[18,5], hr.pa.aging = final.frame[18,6], bb.pa.aging = final.frame[18,7], ibb.pa.aging = final.frame[18,8],
                                                   hbp.pa.aging = final.frame[18,9], sf.pa.aging = final.frame[18,10], bat.pa.aging = final.frame[18,11], fld.pa.aging = final.frame[18,12],
                                                   bsr.pa.aging = final.frame[18,13]))
    }
  }

  for (i in 18:21){
    if (!(i %in% final.frame$post.age)){
      final.frame <- rbind(final.frame, data.frame(post.age = i, wOBA.aging = final.frame[1,2], b1.pa.aging = final.frame[1,3], b2.pa.aging = final.frame[1,4],
                                                   b3.pa.aging = final.frame[1,5], hr.pa.aging = final.frame[1,6], bb.pa.aging = final.frame[1,7], ibb.pa.aging = final.frame[1,8],
                                                   hbp.pa.aging = final.frame[1,9], sf.pa.aging = final.frame[1,10], bat.pa.aging = final.frame[1,11], fld.pa.aging = final.frame[1,12],
                                                   bsr.pa.aging = final.frame[1,13]))
    }
  }



  final.frame <- final.frame[order(final.frame$post.age),]

  return(final.frame)

}
