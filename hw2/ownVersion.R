n <- 20

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
sum(df$nE)

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
hist(df$Ms, xlab="Money",main="Histogram for split-halve",
     col=rgb(0,0,1,0.2),
     xlim=c(0,max(c(df$Me,df$Ms)))*1.1,ylim=c(0,nrow(df)/2))
hist(df$Me, col=rgb(0,1,0,0.2),
     add=TRUE)
legend('topright', c('Money befor', 'Money after'),
       fill=c(rgb(0,0,1,0.5), rgb(0,1,0,0.2)))

plot(df$ID,df$nE,main="Number of Changes",
     xlab="ID",ylab="Number of Changes",
     col=rgb(1,0,0,0.2))


plot(df$nE,df$Me,main="Correlation?",
     xlab="Number of Changes",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

plot(df$Ms,df$Me,main="Correlation?",
     xlab="Money at the Beginning",ylab="Money at the End",
     col=rgb(1,0,0,0.2))

plot(df$ID,df$Me,main="Correlation?",
     xlab="ID",ylab="Money at the End",
     col=rgb(1,0,0,0.2))
