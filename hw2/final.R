library(gt)
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)



nA = 1000         # number of Agents
ID = seq_len(nA)  # ID of the Agents
M0pop = 100       # Mean amount of Money in the Start-Population


# generating Start-Population

PopNorm <- data.frame( ID = ID,
                       Money= sort(rnorm(nA, mean = M0pop, sd = 0.2 * M0pop))
                      )

ggplot(PopNorm, aes(x = Money)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50, fill = 4) +
  ylab("Frequency") +
  scale_fill_manual(values = 4) +
  theme_minimal() +
  theme(legend.position = "top")

# generating random split


splitopt <- function(){
  rn <- runif(1)
  c(rn,1-rn)
}


# calculating the Probability for the next exchange

calc_p <- function(M_dist) {
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
      Pot <- max(M_A + M_oA[ii], 10e-8)
      pwin[ii] <- 1-min(1, M_A/Pot)
      pmed[ii] <- 1-min(1, M_Med/Pot)
      pmean[ii] <- 1-min(1, M_Mean/Pot)
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


calc_p <- function(VM, Detailed=FALSE) {
  n <- length(VM)
  ID <- seq(1:n)
  MM <- matrix(rep(VM,times=n),n,n)
  S <- (VM+t(MM))
  S[S==0] <- 10e-8
  pwin <- (1-t(MM)/S)
  diag(pwin) <- 0
  pmed <- 1-(median(VM)/S)
  pmed[pmed<0] <- 0
  diag(pmed) <- 0
  pmean <- 1-(mean(VM)/S)
  pmean[pmean<0] <- 0
  diag(pmean) <- 0

  Dist <- data.frame(Money = VM,
                     probwin = colSums(pwin)/(n-1),
                     probmed = colSums(pmed)/(n-1),
                     probmean = colSums(pmean)/(n-1)
  )

  Sum <- data.frame(Money = c(min(Dist$Money),
                              max(Dist$Money),
                              median(Dist$Money),
                              mean(Dist$Money)),
                    probwin = c(min(Dist$probwin),
                                max(Dist$probwin),
                                median(Dist$probwin),
                                mean(Dist$probwin)),
                    probmed = c(min(Dist$probmed),
                                max(Dist$probmed),
                                median(Dist$probmed),
                                mean(Dist$probmed)),
                    probmean = c(min(Dist$probmean),
                                 max(Dist$probmean),
                                 median(Dist$probmean),
                                 mean(Dist$probmean))
  )

  rownames(Sum) <- c("min", "max", "med", "mean")
  Output <- list(Sum = Sum, Dist = Dist)
  if (Detailed) {
    D <- data.frame(t(pmed))
    diag(D) <- colSums(pmed)/(n-1)
    D <- round(D, digits=2)
    D <- format(D, digits=2)
    VMm <- round(median(VM), digits=1)
    VMm <- format(VMm, digits=4)
    VM <- round(VM, digits=1)
    VM <- format(VM, digits=4)
    D <- rbind(VM, D)
    D <- cbind(c("Money",ID),c(VMm,VM), D)
    colnames(D) <- c("ID","Money",ID)
    D <- gt(D)
    D <- tab_spanner(D, label = "Probabilities", columns = 3:(n+2))
    for (i in 1:(n + 1)) {
      if (i==1) {
        D <- tab_style(D,
                       style = list(cell_text(style = "italic")),
                       locations = cells_body(i+1,))
        D <- tab_style(D,
                       style = list(cell_text(style = "italic")),
                       locations = cells_body(,i))
        }
      D <- tab_style(D,
                     style = list(cell_text(weight = "bold")),
                     locations = cells_body(i+1, i))
    }
    D <- tab_source_note(
      D,
      source_note = "Italic/bold = Median of Money, Bold = Populatianmean"
    )
    Output$SumD <- D
  }
  return(Output)
}


M <- c(100,100,100)

DF <- calc_p(M, TRUE)

DF$SumD











SumDf <- rbind(SumD, format(c(Dist$probwin,Dist$probmed,Dist$probmean), digits=2))
rownames(SumD) <- c(ID,"Mean")
colnames(SumD) <- rep(ID,3)
Output$SumD <- data.frame(SumD)


yntest <- 6
df <- prob(rep(100, times = ntest))
round(df$Dist, digits = 2)
round(df$Sum, digits = 2)

df$Dist$Money[1] <- 50
df$Dist$Money[2] <- 150

df <- prob(df$Dist$Money)
round(df$Dist, digits = 2)
round(df$Sum, digits = 2)

df$Dist$Money[3] <- 200
df$Dist$Money[2] <- 50

df <- prob(df$Dist$Money)
round(df$Dist, digits = 2)
round(df$Sum, digits = 2)


ProbNorm <- calc_p(PopNorm$Money)

figProb <- function(Prob, Title, xmax) {
  Figd <- pivot_longer(data.frame(Prob$Dist),
                       cols = starts_with("p"),
                       names_to = "Outcome",
                       values_to = "Probability"
                       )
  Figs <- Prob$Sum

  Figp <- ggplot(data = Figd,
                 aes(x = Money,
                     y = Probability,
                     color = Outcome
                     )
                 ) +
    geom_point( alpha = 0.5, size = 1.5) +
    scale_color_manual(name = "Probability to",
                       values = c(2, 3, 4),
                       labels = c("gain more than the Mean",
                                  "gain more than the Median",
                                  "gain")
                       ) +
    ylim(0, 1) +
    xlim(0, xmax) +

    geom_vline(xintercept = Figs["med","Money"],
               linetype = "solid", color = 1) +
    annotate("text",
             x = Figs["med","Money"] * 0.95,
             y = 0,
             hjust = 1,
             vjust = 0,
             label = paste("Median =",
                           round(Figs["med","Money"], 0)),
             color = 1) +

    geom_vline(xintercept = Figs["mean","Money"],
               linetype = "dashed", color = 1) +
    annotate("text",
             x = Figs["mean","Money"] * 1.05,
             y = 0,
             hjust = 0,
             vjust = 0,
             label = paste("Mean =",
                           round(Figs["mean","Money"], 0)),
             color = 1) +

    geom_hline(yintercept = Figs["mean","probmed"],
             linetype = "solid", color = 3) +
    annotate("text",
             x = xmax,
             y = Figs["mean","probmed"] * 1.05,
             hjust = 1,
             vjust = 0,
             label = paste("Mean =",
                           round(Figs["mean","probmed"], 2)),
             color = 3) +

    geom_hline(yintercept = Figs["mean","probmean"],
               linetype = "solid", color = 2) +
    annotate("text",
             x = xmax,
             y = Figs["mean","probmean"]*0.95,
             hjust = 1,
             vjust = 1,
             label = paste("Mean =",
                           round(Figs["mean","probmean"], 2)),
             color = 2) +

    labs(title = Title) +
    theme_light() +
    theme(legend.position = "bottom")
  return(Figp)
}

figProb(ProbNorm, "Probability at Beginning", 250)

# Simulation


ecosim <- function( n, M_dist, TL = 0 ) {
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
    rds <- splitopt()
    df[rdf,"nE"] <- df[rdf,"nE"] + 1
    df[rdf,"MT_E"] <- sum( df[rdf,"MT_E"]) * rds
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



df <- ecosim(50000, PopNorm$Money, TL = 500)

summary(df$Sum)
df$Timeline

figHist <- function(sim_sum) {
  Fig <- pivot_longer(data.frame(sim_sum),
                      cols = starts_with("MT"),
                      names_to = "Distribution",
                      values_to = "Money"
                      )
  Fig$Distribution <-recode(Fig$Distribution,
                            "MT_S" = "at Begining",
                            "MT_E" = "at the End"
                            )
  Figp <- ggplot(Fig02, aes(x = Money, fill = Distribution)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
    ylab("Frequency") +
    scale_fill_manual(name = NULL, values = c(4,3)) +
    scale_color_manual(name = NULL, values = c(4, 3)) +
    theme_minimal() +
    theme(legend.position = "top")
  return(Figp)
}

figHist(df$Sum)

MTime <- data.frame(ID,df$Timeline)

sID <- c(1,2,3,nA/2-1,nA/2,nA/2+1,nA-2,nA-1,nA)
sMTime <- MTime[sID,]

Fig03 <- pivot_longer(data.frame(sMTime),
                      cols = !matches("ID"),
                      names_to = "Time",
                      names_prefix = "n",
                      names_transform = list(Time = as.integer),
                      values_to = "Money"
)

Fig03$ID <- sprintf("%04d", Fig03$ID)

ggplot(data = Fig03, aes(x = Time, y = Money, color = ID)) +
  geom_line() +
  ggtitle("Timeline for seven Agents") +
  xlab("number of exchanges") +
  labs(color = "Agents ID") +
  scale_color_manual(values = c(2,2,2, 1,1, 1,3,3,3)) +
  theme_light() +
  theme(legend.position = "right",
        legend.background = element_rect(fill = "white"))



FigA <- figProb(calc_p(MTime[,2]), "Probability at Beginning", 700)
FigB <- figProb(calc_p(MTime[,3]), "Probability after 500 exchanges", 700)
FigC <- figProb(calc_p(MTime[,4]), "Probability after 1000 exchanges", 700)
FigD <- figProb(calc_p(MTime[,NCOL(MTime)]), "Probability at the End", 700)


(FigA + FigB) / (FigC + FigD)



calc_p_t <- function(M_dist) {
  pnt <- prob(M_dist[,1])
  tx <- as.numeric(gsub("n", "",colnames(M_dist)[1]))
  pnts <- data.frame(Time = rep(tx, times = 4),
                     Res = colnames(pnt$Sum),
                     t(pnt$Sum)
                     )
  for(i in 2:ncol(M_dist)) {
    pnt <- prob(M_dist[,i])
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

probNormt <- calc_p_t(df$Timeline)
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
  scale_color_manual(name = NULL, values = c(2, 3, 4),
                     labels = c("gain more than the Mean",
                                "gain more than the Median",
                                "gain")) +
  ggtitle("Probability to ... of Money - Timeline") +
  xlab("number of exchanges") +
  ylab("Mean-Probability in the Population") +
  ylim(0.35, 0.55) +
  theme_light() +
  theme(legend.position = "bottom",
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white"))



