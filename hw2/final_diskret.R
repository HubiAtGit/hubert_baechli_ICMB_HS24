library(tidyr)
library(dplyr)
library(ggplot2)

nA = 500          # number of Agents
ID = seq_len(nA)  # ID of the Agents
M0pop = 100       # Mean amount of Money in the Start-Population


# generating Start-Population

CHF_T0 <- data.frame( ID = ID,
                      PopNorm = sort(
                                 rnorm(nA, mean = M0pop, sd = 0.2 * M0pop)
                                ),
                       PopUnif = sort(
                                  runif(nA, min = 0, max = M0pop *2)
                                ),
                       PopEven = rep(M0pop, times = nA)
                       )

# ploting Start-Population

Fig01 <- pivot_longer(data.frame(CHF_T0),
                      cols = !matches("ID"),
                      names_to = "Population",
                      values_to = "Money"
                      )

Fig01$Population <- factor(Fig01$Population,
                           levels = c("PopNorm",
                                      "PopEven",
                                      "PopUnif")
                          )

ggplot(data = Fig01, aes(x = ID, y = Money, color = Population)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Population's",
                     values = c("PopNorm" = 4,
                                "PopEven" = 3,
                                "PopUnif" = 2)) +
  theme_light() +
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white"))

# generating split-options

SplitOpt <- list(Random1 = seq(0, 100) / 100,
                 Random10 = seq(0, 10) / 10,
                 Halve = c( 0.5 )
                 )

# calculating the Probability for the next exchange

prob <- function(M_dist, S_Opt) {
  Dist <- data.frame(Money = M_dist,
                     probwin = 0,
                     probmed = 0,
                     probmean = 0
                     )
  Sum <- data.frame(Money = c(min(Dist$Money),
                              max(Dist$Money),
                              median(Dist$Money),
                              mean(Dist$Money)
                              ),
                    probwin = 0,
                    probmed = 0,
                    probmean = 0
                    )
  rownames(Sum) <- c("min", "max", "med", "mean")
  M_Med <- Sum["med", "Money"]
  M_Mean <- Sum["mean", "Money"]
  for(i in 1:nrow(Dist)) {
    M_A <- Dist[i,"Money"]
    M_oA <- Dist[c(-i),"Money"]
    pwin <- M_oA*0
    pmed <- M_oA*0
    pmean <- M_oA*0
    for(ii in 1:NROW(M_oA)) {
      Pot <- M_A + M_oA[ii]
      M_out <- S_Opt * Pot
      pwin[ii] <- NROW(M_out[ M_out > M_A ]) / NROW(M_out)
      pmed[ii] <- NROW(M_out[ M_out > M_Med ]) / NROW(M_out)
      pmean[ii] <- NROW(M_out[ M_out > M_Mean ]) / NROW(M_out)
    }
    Dist[i,"probwin"] <- mean(pwin)
    Dist[i,"probmed"] <- mean(pmed)
    Dist[i,"probmean"] <- mean(pmean)
  }
  for (i in c("probwin","probmed","probmean")) {
    Sum[[i]] = c(min(Dist[[i]]),
                    max(Dist[[i]]),
                    median(Dist[[i]]),
                    mean(Dist[[i]])
    )
  }

  Output <- list(Sum = Sum, Dist = Dist)
  return(Output)
}

ntest <- 6
df <- prob(rep(100, times = ntest),SplitOpt$Random1)
round(df$Dist, digits = 2)
round(df$Sum, digits = 2)

df$Dist$Money[1] <- 50
df$Dist$Money[2] <- 150

df <- prob(df$Dist$Money,SplitOpt$Random1)
round(df$Dist, digits = 2)
round(df$Sum, digits = 2)

df$Dist$Money[3] <- 200
df$Dist$Money[2] <- 50

df <- prob(df$Dist$Money,SplitOpt$Random1)
round(df$Dist, digits = 2)
round(df$Sum, digits = 2)

CHF_T0[,c("ID", "PopNorm")]

probNorm <- prob(CHF_T0$PopNorm,SplitOpt$Random1)

Fig02 <- pivot_longer(data.frame(probNorm$Dist),
                      cols = starts_with("p"),
                      names_to = "Outcome",
                      values_to = "Probability"
)

ggplot(data = Fig02, aes(x = Money, y = Probability, color = Outcome)) +
  geom_line(linewidth = 1.1) +
  ylim(0, 1) +
  ggtitle("Probability for the Norm-Population") +
  theme_light() +
  theme(legend.position = c(0.7, 0.9),
        legend.background = element_rect(fill = "white"))

probEven <- prob(CHF_T0$PopEven,SplitOpt$Random1)

Fig03 <- pivot_longer(data.frame(ID = ID, probEven$Dist),
                      cols = starts_with("p"),
                      names_to = "Outcome",
                      values_to = "Probability"
)

ggplot(data = Fig03, aes(x = ID, y = Probability, color = Outcome)) +
  geom_line(linewidth = 1.1) +
  ylim(0, 1) +
  ggtitle("Probability for the Even-Population") +
  theme_light() +
  theme(legend.position = c(0.7, 0.9),
        legend.background = element_rect(fill = "white"))

probUnif <- prob(CHF_T0$PopUnif,SplitOpt$Random1)

Fig04 <- pivot_longer(data.frame(probUnif$Dist),
                      cols = starts_with("p"),
                      names_to = "Outcome",
                      values_to = "Probability"
)

ggplot(data = Fig04, aes(x = Money, y = Probability, color = Outcome)) +
  geom_line(linewidth = 1.1) +
  ylim(0, 1) +
  ggtitle("Probability for the Uniform-Population") +
  theme_light() +
  theme(legend.position = c(0.7, 0.9),
        legend.background = element_rect(fill = "white"))


# Simulation


ecosim <- function( n, M_dist, S_Opt, TL = 0 ) {
  df <- data.frame(ID=seq(1,NROW(M_dist)),
                   nE=0,
                   MT_S=M_dist,
                   MT_E=M_dist
                   )
  if (TL > 0) {
    M_TL <- data.frame(n0 = df$MT_S)
  }
  for(i in 1:n) {
    rdf <- sample(df$ID, size=2)
    rds <- sample(S_Opt, size=1)
    df[rdf,"nE"] <- df[rdf,"nE"] + 1
    df[rdf,"MT_E"] <- sum( df[rdf,"MT_E"]) * c(rds, 1-rds)
    if ( TL > 0 ) {
      if ( i %% TL == 0) {
        M_TL [[paste0("n",i)]]<- df$MT_E
      }
    }
  }
  Output <- list("Sum" = df)
  if ( TL > 0 ) {
    rownames(M_TL) <- df$ID
    Output$Timeline <- M_TL
  }
  return(Output)
}



df <- ecosim(7000, CHF_T0$PopNorm, SplitOpt$Random1, TL = 50)

7000/50

summary(df$Sum)
df$Timeline

Fig05 <- pivot_longer(data.frame(df$Sum),
                      cols = starts_with("MT"),
                      names_to = "Distribution",
                      values_to = "Money"
)

Fig05$Distribution <-recode(Fig05$Distribution,
                            "MT_S" = "at Begining",
                            "MT_E" = "at the End")

ggplot(Fig05, aes(x = Money, fill = Distribution)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  ylab("Frequency") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.9),
        legend.background = element_rect(fill = "white", color = NA))



MTime <- data.frame(ID,df$Timeline)

sID <- c(1,2,3,nA/2-1,nA/2,nA/2+1,nA-2,nA-1,nA)
sMTime <- MTime[sID,]

Fig06 <- pivot_longer(data.frame(sMTime),
                      cols = !matches("ID"),
                      names_to = "Time",
                      names_prefix = "n",
                      names_transform = list(Time = as.integer),
                      values_to = "Money"
)

Fig06$ID <- sprintf("%04d", Fig06$ID)

ggplot(data = Fig06, aes(x = Time, y = Money, color = ID)) +
  geom_line() +
  ggtitle("Timeline for seven Agents") +
  xlab("number of exchanges") +
  labs(color = "Agents ID") +
  scale_color_manual(values = c(2,2,2, 1,1, 1,3,3,3)) +
  theme_light() +
  theme(legend.position = "right",
        legend.background = element_rect(fill = "white"))

pT <- 2
pT <- 7
pT <- 12
pT <- NCOL(MTime)


probNormT <- prob(MTime[,pT],SplitOpt$Random1)


Fig07 <- pivot_longer(data.frame(probNormT$Dist),
                      cols = starts_with("p"),
                      names_to = "Outcome",
                      values_to = "Probability"
)

Fig07l <- probNormT$Sum

Fig07l["med","Money"]

subTitle <- gsub("n", "",names(MTime)[pT])


ggplot(data = Fig07, aes(x = Money, y = Probability, color = Outcome)) +
  geom_point( alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c(2, 3, 1)) +
  ylim(0, 1) +
  xlim(0, 5500) +

  geom_vline(xintercept = PopMed,
             linetype = "solid",
             color = "black") +
  annotate("text",
           x = Fig07l["med","Money"],
           y = 0,
           hjust = 0,
           label = paste(" Population Median =", round(Fig07l["med","Money"], 0)),
           color = "black") +

  geom_vline(xintercept = Fig07l["mean","Money"],
             linetype = "dashed",
             color = "black") +
  annotate("text",
           x = PopMean,
           y = 0.08,
           hjust = 0,
           label = paste(" Population Mean =", round(Fig07l["mean","Money"], 0)),
           color = "black") +

  geom_hline(yintercept = Fig07l["mean","probmed"],
             linetype = "solid",
             color = 3) +
  annotate("text",
           x = 5500,
           y = Fig07l["mean","probmed"] * 1.05,
           hjust = 1,
           vjust = 0,
           label = paste(" Population Probability =", round(Fig07l["mean","probmed"], 2)),
           color = 3) +

  geom_hline(yintercept = Fig07l["mean","probmean"],
             linetype = "solid",
             color = 2) +
  annotate("text",
           x = 5500,
           y = Fig07l["mean","probmean"]*0.95,
           hjust = 1,
           vjust = 1,
           label = paste(" Population Probability =", round(Fig07l["mean","probmean"], 2)),
           color = 2) +

  labs(title = "Probability for the Norm-Population") +
  annotate("text",
           x = 0,
           y = 1,
           hjust = 0,
           vjust = 0,
           label = paste(" (number of exchanges = ", subTitle, ")"),
           color = "black") +

  theme_light() +
  theme(legend.position = c(0.93, 0.95),
        legend.background = element_rect(fill = "white"))



probt <- function(M_dist, S_Opt) {
  pnt <- prob(M_dist[,1],S_Opt)
  tx <- as.numeric(gsub("n", "",colnames(M_dist)[1]))
  pnts <- data.frame(Time = rep(tx, times = 4),
                     Res = colnames(pnt$Sum),
                     t(pnt$Sum)
                     )
  for(i in 2:ncol(M_dist)) {
    pnt <- prob(M_dist[,i],S_Opt)
    tx <- as.numeric(gsub("n", "",colnames(M_dist)[i]))
    pnti <- data.frame(Time = rep(tx, times = 4),
                       Res = colnames(pnt$Sum),
                       t(pnt$Sum)
                       )
    pnts <- rbind(pnts,pnti)
  }
  rownames(pnts) <- NULL
  return(pnts)
}

probNormt <- probt(df$Timeline,SplitOpt$Random1)
probNormt


Fig08 <- probNormt[probNormt$Res=="Money",]

ggplot(data = Fig08, aes(x = Time)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = 4, alpha = 0.1) +
  geom_line(aes(y = min), color = 4, linetype = "dotted", size = 0.8) +
  geom_line(aes(y = max), color = 4, linetype = "dotted", size = 0.8) +
  geom_line(aes(y = mean, color = "Mean", linetype = "Mean"), size = 1) +
  geom_line(aes(y = med, color = "Median", linetype = "Median"), size = 1) +
  ggtitle("Timeline - Money") +
  xlab("number of exchanges") +
  ylab("Money") +
  scale_color_manual(name = "Population", values = c("Mean" = 4, "Median" = 4)) +
  scale_linetype_manual(name = "Population", values = c("Mean" = "dashed", "Median" = "solid")) +
  theme_light() +
  theme(legend.position = c(0.01, 1),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white"))

Fig09 <- probNormt[probNormt$Res=="probwin",]

ggplot(data = Fig09, aes(x = Time)) +
  geom_line(aes(y = mean, color = "Mean", linetype = "Mean"), size = 1) +
  geom_line(aes(y = med, color = "Median", linetype = "Median"), size = 1) +
  ggtitle("Timeline - Probability to gain Money") +
  xlab("number of exchanges") +
  ylab("Probability") +
  ylim(0.45, 0.55) +
  scale_color_manual(name = "Population", values = c("Mean" = 1, "Median" = 1)) +
  scale_linetype_manual(name = "Population", values = c("Mean" = "dashed", "Median" = "solid")) +
  theme_light() +
  theme(legend.position = c(0.01, 1),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white"))

Fig10 <- probNormt[probNormt$Res=="probmed",]

ggplot(data = Fig10, aes(x = Time)) +
  geom_line(aes(y = mean, color = "Mean", linetype = "Mean"), size = 1) +
  geom_line(aes(y = med, color = "Median", linetype = "Median"), size = 1) +
  ggtitle("Timeline - Probability to gain more than the Median of Money") +
  xlab("number of exchanges") +
  ylab("Probability") +
  ylim(0.45, 0.55) +
  scale_color_manual(name = "Population", values = c("Mean" = 3, "Median" = 3)) +
  scale_linetype_manual(name = "Population", values = c("Mean" = "dashed", "Median" = "solid")) +
  theme_light() +
  theme(legend.position = c(0.01, 1),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white"))

Fig11 <- probNormt[probNormt$Res=="probmean",]

ggplot(data = Fig11, aes(x = Time)) +
  geom_line(aes(y = mean, color = "Mean", linetype = "Mean"), size = 1) +
  geom_line(aes(y = med, color = "Median", linetype = "Median"), size = 1) +
  ggtitle("Timeline - Probability to gain more than the Mean of Money") +
  xlab("number of exchanges") +
  ylab("Probability") +
  ylim(0.25, 0.55) +
  scale_color_manual(name = "Population", values = c("Mean" = 2, "Median" = 2)) +
  scale_linetype_manual(name = "Population", values = c("Mean" = "dashed", "Median" = "solid")) +
  theme_light() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white"))

Fig12 <- probNormt[probNormt$Res!="Money",c("Time","Res","mean")]

ggplot(data = Fig12, aes(x = Time, y = mean, color = Res)) +
  geom_line(size = 1) +
  scale_color_manual(name = NULL, values = c(2, 3, 1),
                     labels = c("gain more than the Mean",
                                "gain more than the Median",
                                "gain")) +
  ggtitle("Probability to ... of Money - Timeline") +
  xlab("number of exchanges") +
  ylab("Mean-Probability in the Population") +
  ylim(0.35, 0.55) +
  theme_light() +
  theme(legend.position = c(0.01, 1),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white"))



