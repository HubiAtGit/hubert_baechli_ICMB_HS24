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

df <- SplitHalve(1000,20)


SplitHalve2 <- function(nAgent,maxExchange,Money_mean=1000,Money_sd=300) {
  df <- data.frame(ID=seq(1,nAgent),
                   nE=0,
                   Ms=rnorm(nAgent, mean = Money_mean, sd = Money_sd),
                   MT=0,Me=0)
  df$Me <- df$Ms
  iE <- 0
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

df <- SplitHalve2(1000,10000)
summary(df)
hist(df$Ms, xlab="Money",main="Histogram for split-halve",
     col=rgb(0,0,1,0.2),
     xlim=c(0,max(c(df$Me,df$Ms)))*1.1,ylim=c(0,nrow(df)/2))
hist(df$Me, col=rgb(1,0,0,0.2),
     add=TRUE)
legend('topright', c('Money befor', 'Money after'),
       fill=c(rgb(0,0,1,0.5), rgb(1,0,0,0.2)))

hist(df$nE, xlab="Money",main="Number of Exchanges",
     col=rgb(0,0,1,0.2),
     xlim=c(0,max(df$nE))*1.1)
