

SplitHalve <- function(nAgent,maxExchange,Money_mean=1000,Money_sd=300) {
  df <- data.frame(ID=seq(1,nAgent),
                   nE=0,
                   Ms=rnorm(nAgent, mean = Money_mean, sd = Money_sd),
                   MT=0,Me=0)
  df$Me <- df$Ms
  iE <- 1
  while (iE <= maxExchange) {
    df$MT <- df$Me
    rdf <- sample(df[df$nE < iE,"ID"], size=2)
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"] <-
      df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"]+1
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"Me"] <-
      sum(df[(df$ID == rdf[1] | df$ID == rdf[2]),"MT"])/2
    if (NROW(df[df$nE < iE,"ID"]) < 2) {
      iE <- iE+1
      }
  }
  return(df)
}

df <- SplitHalve(1000,30)
sum(df$Me)

SplitHalve2 <- function(nAgent,maxExchange,Money_mean=1000,Money_sd=300) {
  df <- data.frame(ID=seq(1,nAgent),
                   nE=0,
                   Ms=rnorm(nAgent, mean = Money_mean, sd = Money_sd),
                   MT=0,Me=0)
  df$Me <- df$Ms
  iE <- 1
  while (iE <= maxExchange) {
    df$MT <- df$Me
    rdf <- sample(df$ID, size=2)
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"] <-
      df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"]+1
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"Me"] <-
      sum(df[(df$ID == rdf[1] | df$ID == rdf[2]),"MT"])/2
    iE <- iE+1
  }
  return(df)
}

df <- SplitHalve2(1000,15000)

SplitRandom <- function(nAgent,maxExchange,Money_mean=1000,Money_sd=300) {
  df <- data.frame(ID=seq(1,nAgent),
                   nE=0,
                   Ms=rnorm(nAgent, mean = Money_mean, sd = Money_sd),
                   MT=0,Me=0)
  df$Me <- df$Ms
  iE <- 1
  while (iE <= maxExchange) {
    df$MT <- df$Me
    rdf <- sample(df$ID, size=2)
    rds <- sample(0:100, size=1)/100
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"] <-
      df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"]+1
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"Me"] <-
      sum(df[(df$ID == rdf[1] | df$ID == rdf[2]),"MT"])*c(rds,1-rds)
    iE <- iE+1
  }
  return(df)
}

sample ( 0:100 , 1 )/100
30*c(0.2,0.8)

df <- SplitRandom(1000,15000)

summary(df)
hist(df$Ms, xlab="Money",main="Histogram for split-random",
     col=rgb(0,0,1,0.2),
     xlim=c(0,max(c(df$Me,df$Ms))*1.1),ylim=c(0,nrow(df)/2))
hist(df$Me, col=rgb(0,1,0,0.2),
     add=TRUE)
legend('topright', c('Money befor', 'Money after'),
       fill=c(rgb(0,0,1,0.2), rgb(0,1,0,0.2)))

plot(df$ID,df$nE,main="Number of Changes",
     xlab="ID",ylab="Number of Changes",
     col=rgb(1,0,0,0.2))


plot(df$nE,df$Me,main="Correlation to the Number of Changes",
     xlab="Number of Changes",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

plot(df$Ms,df$Me,main="Correlation to the Money at the Beginning",
     xlab="Money at the Beginning",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

plot(df$ID,df$Me,main="Correlation to the ID",
     xlab="ID",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

SplitRandom2 <- function(nAgent,maxExchange,Money_mean=1000,Money_sd=300) {
  df <- data.frame(ID=seq(1,nAgent),
                   nE=0,
                   Ms=rnorm(nAgent, mean = Money_mean, sd = Money_sd),
                   MT=0,Me=0)
  df$Me <- df$Ms
  iE <- 1
  while (iE <= maxExchange) {
    df$MT <- df$Me
    rdf <- sample(df$ID, size=2)
    rds <- sample(30:70, size=1)/100
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"] <-
      df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"]+1
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"Me"] <-
      sum(df[(df$ID == rdf[1] | df$ID == rdf[2]),"MT"])*c(rds,1-rds)
    iE <- iE+1
  }
  return(df)
}

df <- SplitRandom2(1000,60000)

summary(df)
hist(df$Ms, xlab="Money",main="Histogram for split-random (30%-70%)",
     col=rgb(0,0,1,0.2),
     xlim=c(0,max(c(df$Me,df$Ms)))*1.1,ylim=c(0,nrow(df)/2))
hist(df$Me, col=rgb(0,1,0,0.2),
     add=TRUE)
legend('topright', c('Money befor', 'Money after'),
       fill=c(rgb(0,0,1,0.5), rgb(0,1,0,0.2)))


plot(df$nE,df$Me,main="Correlation?",
     xlab="Number of Changes",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

plot(df$Ms,df$Me,main="Correlation?",
     xlab="Money at the Beginning",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

plot(df$ID,df$Me,main="Correlation?",
     xlab="ID",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

SplitRandomR <- function(nAgent,maxExchange,Money_mean=1000,Money_sd=300) {
  df <- data.frame(ID=seq(1,nAgent),
                   nE=0,
                   Ms=rnorm(nAgent, mean = Money_mean, sd = Money_sd),
                   MT=0,Me=0)
  df$Me <- df$Ms
  iE <- 1
  while (iE <= maxExchange) {
    df$MT <- df$Me
    rdf <- sample(df[df$nE < iE,"ID"], size=2)
    rds <- sample(1:100, size=1)/100
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"] <-
      df[(df$ID == rdf[1] | df$ID == rdf[2]),"nE"]+1
    df[(df$ID == rdf[1] | df$ID == rdf[2]),"Me"] <-
      sum(df[(df$ID == rdf[1] | df$ID == rdf[2]),"MT"])*c(rds,1-rds)
    if (NROW(df[df$nE < iE,"ID"]) < 2) {
      iE <- iE+1
    }
  }
  return(df)
}

df <- SplitRandomR(100,30)

summary(df)
hist(df$Ms, xlab="Money",main="Histogram for split-random Restricted",
     col=rgb(0,0,1,0.2),
     xlim=c(0,max(c(df$Me,df$Ms)*1.1)),ylim=c(0,nrow(df)/2))
hist(df$Me, col=rgb(0,1,0,0.2),
     add=TRUE)
legend('topright', c('Money befor', 'Money after'),
       fill=c(rgb(0,0,1,0.5), rgb(0,1,0,0.2)))

plot(df$ID,df$nE,main="Number of Changes",
     xlab="ID",ylab="Number of Changes",
     col=rgb(1,0,0,0.2))




win_prob <- function( Moneyvec ) {
  df <- data.frame(Money=Moneyvec,
                   winprobability=0)
  for(i in 1:nrow(df)) {
    MA <- df[i,1]
    MoA <- df[c(-i),1]
    WP <- MoA*0
    for(ii in 1:NROW(MoA)) {
      Sum_M <- MA + MoA[ii]
      if (Sum_M == 0) {
        WP[ii] <- 1.00
      } else {
        WP[ii] <- WP[ii] <- 1-min(1, MA/Sum_M)
      }
    }
    df[i,2] <- round(mean(WP), digits =3)
  }
  return(df)
}

win_stab_prob <- function( Moneyvec ) {
  df <- data.frame(Money=Moneyvec,
                   winprobability=0)
  MMA <- median(df[, 1])
  for(i in 1:nrow(df)) {
    MA <- df[i,1]
    MoA <- df[c(-i),1]
    WP <- MoA*0
    for(ii in 1:NROW(MoA)) {
      Sum_M <- MA + MoA[ii]
      if (Sum_M == 0) {
        WP[ii] <- 1.00
      } else {
        WP[ii] <- WP[ii] <- 1-min(1, MMA/Sum_M)
      }
    }
    df[i,2] <- round(mean(WP), digits =3)
  }
  return(df)
}

A <- c(5,10,11)

win_prob(A)
win_stab_prob(A)

df1_even <- SplitRandomR(1000,5,1000,250)

win_prob(df1_even$Ms)[, 2]

plot(df1_even$Ms,win_prob(df1_even$Ms)[, 2])
plot(df1_even$Me,win_prob(df1_even$Me)[, 2])

mean(win_prob(df1_even$Ms)[, 2])

win_stab_prob(df1_even$Ms)[, 2]
mean(win_stab_prob(df1_even$Ms)[, 2])

win_prob(df1_even$Me)[, 2]
mean(win_prob(df1_even$Me)[, 2])

win_stab_prob(df1_even$Me)[, 2]
mean(win_stab_prob(df1_even$Me)[, 2])

