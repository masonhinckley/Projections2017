### Relief Pitcher Aging Factors

rp_aging_factors <- function(){

  library(dplyr)
  library(XML)

  raw.data <- readHTMLTable("http://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=y&type=c,3,6,14,19,20,21,24,48,49,120,121&season=2014&month=0&season1=2003&ind=1&team=0&rost=0&age=0&filter=&players=0&page=1_2000", stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
  for (i in c(1:2,5:11)){
    class(raw.data[,i]) <- "numeric"
  }

  names(raw.data)[12:15] <- c("GB.","FB.", "K.", "BB.")

  raw.data$GB. <- as.numeric(sub("%", "", raw.data$GB.))/100
  raw.data$FB. <- as.numeric(sub("%", "", raw.data$FB.))/100
  raw.data$BB. <- as.numeric(sub("%", "", raw.data$BB.))/100
  raw.data$K. <- as.numeric(sub("%", "", raw.data$K.))/100
  raw.data <- mutate(raw.data, BIP. = (TBF - SO - BB - IBB - HBP)/TBF)

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

        age.frame <- rbind(age.frame, data.frame(Name = temp.player, ERA.diff = post.player$ERA - pre.player$ERA,
                                                 BIP.diff = post.player$BIP. - pre.player$BIP.,
                                                 GB.diff = post.player$GB. - pre.player$GB.,
                                                 FB.diff = post.player$FB. - pre.player$FB.,
                                                 K.diff = post.player$K. - pre.player$K.,
                                                 BB.diff = post.player$BB. - pre.player$BB.,
                                                 weight = 2/((1/pre.player$TBF) + (1/post.player$TBF))))

      }

      age.frame$ERA.diff <- age.frame$ERA.diff*age.frame$weight
      age.frame$BIP.diff <- age.frame$BIP.diff*age.frame$weight
      age.frame$GB.diff <- age.frame$GB.diff*age.frame$weight
      age.frame$FB.diff <- age.frame$FB.diff*age.frame$weight
      age.frame$K.diff <- age.frame$K.diff*age.frame$weight
      age.frame$BB.diff <- age.frame$BB.diff*age.frame$weight

      temp.frame <- rbind(temp.frame, data.frame(post.age = post.age, ERA.aging = sum(age.frame$ERA.diff)/sum(age.frame$weight),
                                                 BIP.aging = sum(age.frame$BIP.diff)/sum(age.frame$weight),
                                                 GB.aging = sum(age.frame$GB.diff)/sum(age.frame$weight),
                                                 FB.aging = sum(age.frame$FB.diff)/sum(age.frame$weight),
                                                 K.aging = sum(age.frame$K.diff)/sum(age.frame$weight),
                                                 BB.aging = sum(age.frame$BB.diff)/sum(age.frame$weight)))
    }

  }

  eval.num <- ((max(temp.frame$post.age) - min(temp.frame$post.age))*2) + 1

  lw.era <- loess.smooth(temp.frame$post.age, temp.frame$ERA.aging, evaluation = eval.num)
  lw.bip <- loess.smooth(temp.frame$post.age, temp.frame$BIP.aging, evaluation = eval.num)
  lw.gb <- loess.smooth(temp.frame$post.age, temp.frame$GB.aging, evaluation = eval.num)
  lw.fb <- loess.smooth(temp.frame$post.age, temp.frame$FB.aging, evaluation = eval.num)
  lw.k <- loess.smooth(temp.frame$post.age, temp.frame$K.aging, evaluation = eval.num)
  lw.bb <- loess.smooth(temp.frame$post.age, temp.frame$BB.aging, evaluation = eval.num)

  final.frame <- data.frame(post.age = lw.era[[1]][seq(1,eval.num,2)], ERA.aging = lw.era[[2]][seq(1,eval.num,2)],
                            BIP.aging = lw.bip[[2]][seq(1,eval.num,2)], GB.aging = lw.gb[[2]][seq(1,eval.num,2)],
                            FB.aging = lw.fb[[2]][seq(1,eval.num,2)], K.aging = lw.k[[2]][seq(1,eval.num,2)],
                            BB.aging = lw.bb[[2]][seq(1,eval.num,2)])

  for (i in 18:50){
    if (!(i %in% final.frame$post.age)){
      final.frame <- rbind(final.frame, data.frame(post.age = i, ERA.aging = 0, BIP.aging = 0, GB.aging = 0,
                                                   FB.aging = 0, K.aging = 0, BB.aging = 0))
    }
  }

  return(final.frame)


}
