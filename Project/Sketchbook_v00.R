library(tidyverse)

nA = 5            # number of Agents
ID = seq_len(nA)  # ID of the Agents

Pop <- tibble( ID = ID )

setKnowlege <- function(Pop = Pop,
                        Typ = FALSE,
                        K = Knowlege) {
  Kname <- "Knowlege"
  if (Typ != FALSE) {
    Kname <- paste(Kname, Typ, sep = "_")
  }
  if (Kname %in% colnames(Pop)) {
    Pop <- Pop %>%
      mutate(!!Kname := K)
  } else {
    Pop[[Kname]] <- K
  }
  Pop <- Pop %>%

  return(Pop)
}

Pop <- setKnowlege( Pop = Pop, K = 0.5 )
Pop

updateKnowledge <- function(Pop = Pop,
                            Typ = FALSE,
                            add = 0,
                            fac = 1) {
  Kname <- "Knowledge"
  if (Typ != FALSE) {
    Kname <- paste(Kname, Typ, sep = "_")
  }
  if (Kname %in% colnames(Pop)) {
    Pop <- Pop %>%
      mutate( !!Kname := ( .data[[Kname]] + add ) * fac )
  } else {
    Pop <- setKnowlege(Pop = Pop, K = add, Typ = Typ)
    Pop <- Pop %>%
      mutate( !!Kname := .data[[Kname]] * fac )
  }
  return(Pop)
}

Pop <- updateKnowlege( Pop = Pop, add = -0.1 )
Pop

setTimeRes <- function(Pop = Pop,
                       TR = TimeRessource) {
  TRname <- "TimeRessource"
  if (TRname %in% colnames(Pop)) {
    Pop <- Pop %>%
      mutate(!!TRname := TR)
  } else {
    Pop[[TRname]] <- TR
  }
  return(Pop)
}

Pop <- setTimeRes( Pop = Pop, TR = 8 )
Pop

updateTimeRes <- function(Pop = Pop,
                          TR = TimeRessource) {
  TRname <- "TimeRessource"
  if (TRname %in% colnames(Pop)) {
    Pop <- Pop %>%
      mutate( !!TRname := .data[[TRname]] + TR )
  } else {
    Pop <- setTimeRes(Pop = Pop, TR = TR )
  }
  return(Pop)
}

Pop <- updateTimeRes( Pop = Pop, TR = -3 )
Pop

setStudyTime <- function(Pop = Pop,
                         ST = 0) {
  STname <- "StudyTime"
  if (STname %in% colnames(Pop)) {
    Pop <- Pop %>%
      mutate(!!STname := ST)
  } else {
    Pop[[STname]] <- ST
  }
  return(Pop)
}

Pop <- setStudyTime( Pop = Pop )
Pop

set_LearningRate <- function(Pop = Pop,
                            LR = LearningRate) {
  LRname <- "LearningRate"
  if (LRname %in% colnames(Pop)) {
    Pop <- Pop %>%
      mutate(!!LRname := LR)
  } else {
    Pop[[LRname]] <- LR
  }
  return(Pop)
}

Pop <- setLearningRate( Pop = Pop, LR = 0.1 )
Pop

updateLearningRate_Knowlege <- function(Pop = Pop,
                                        Typ = FALSE) {
  LR <- "LearningRate"
  Kname <- "Knowlege"
  if (Typ != FALSE) {
    Kname <- paste(Kname, Typ, sep = "_")
  }
  if (Kname %in% colnames(Pop)) {
    Pop <- Pop %>%
      mutate( !!LR := .data[[Kname]] * 0.5 )
  }
  return(Pop)
}

Pop <- updateLearningRate_Knowlege( Pop = Pop )
Pop

learning_Knowledge <- function(Pop = Pop,
                              Typ = FALSE,
                              LR = FALSE,
                              ST = StudyTime) {
  Kname <- "Knowledge"
  if (Typ != FALSE) {
    Kname <- paste(Kname, Typ, sep = "_")
  }
  if (Kname %in% colnames(Pop)) {
    K <- Pop[[Kname]]
  }
  if (LR == FALSE) {
    if (Kname %in% colnames(Pop)) {
      LR <- Pop[["LearningRate"]]
    }
  }
  if ("StudyTime" %in% colnames(Pop)) {
    Pop[["StudyTime"]] <- Pop[["StudyTime"]] + ST
  }
  T0 <- ( 1 - K )^( 1 / -LR )   # assumed time learnd allready
  K <- 1 - ( T0 + ST )^( -LR )  # Knowlege after time learnd
  Pop[[Kname]] <- K
  return(Pop)
}

Pop <- learning_Knowlege( Pop = Pop, ST = 1 )
Pop

sel_rnd_SubPop <- function(Pop = Pop,
                           n = 2) {
  rdf <- sample( Pop[["ID"]], size=n )
  SubPop <- list()
  SubPop$sel <- Pop %>%
    filter(ID %in% rdf)
  SubPop$rest <- Pop %>%
    filter(!ID %in% rdf)
  return(SubPop)
}

integrate_SubPop <- function(Pop = Pop,
                             SubPop = SubPop) {
  for (i in SubPop[["ID"]]) {
    Pop[Pop$ID == i,] <- SubPop[SubPop$ID == i,]
  }
  return(Pop)
}

integrate_SubPop(Pop = Pop, SubPop = SubPop)

sim_rnd <- function(Pop = Pop,
                    Typ = FALSE,
                    LR = FALSE,
                    STn = 1,
                    n = NumberMeetings) {

  if (LR == FALSE) {
    Pop <- updateLearningRate_Knowlege(Pop = Pop,
                                       Typ = Typ)
  } else {
    Pop <- setLearningRate(Pop = Pop, LR = LR)
  }

  Pop <- setStudyTime( Pop = Pop )

  Kname <- "Knowlege"
  if (Typ != FALSE) {
    Kname <- paste(Kname, Typ, sep = "_")
  }

  TL <- tibble( ID = Pop[["ID"]] )
  i <- 0
  iname <- sprintf("%08d", i)
  TL[[iname]] <- Pop[[Kname]]
  M <- tibble( Time = i,
               Mean = mean(Pop[[Kname]]))

  for(i in 1:n) {
    SubPop <- sel_rnd_SubPop(Pop = Pop, n = 2)$sel
    SubPop <- learning_Knowlege(SubPop,
                                Typ = Typ,
                                ST = STn,
                                LR = mean(SubPop[["LearningRate"]]))
    if (LR == FALSE) {
      SubPop <- updateLearningRate_Knowlege(Pop = SubPop,
                                            Typ = Typ)
    }
    Pop <- integrate_SubPop(Pop = Pop, SubPop = SubPop)
    iname <- sprintf("%08d", i)
    TL[[iname]] <- Pop[[Kname]]
    M[i + 1, 1] <- i
    M[i + 1, 2] <- mean(Pop[[Kname]])
  }
  TL <- pivot_longer(data.frame(TL),
                     cols = !matches("ID"),
                     names_to = "Time",
                     names_prefix = "X",
                     names_transform = list(Time = as.integer),
                     values_to = "Knowlege"
                     )
  TL$ID <- sprintf("%04d", TL$ID)
  TL <-right_join(TL, M, by="Time")
  Output <- list(Pop=Pop,
                 TL=TL)
  return(Output)
}

nA = 100          # number of Agents
ID = seq_len(nA)  # ID of the Agents
Pop <- tibble( ID = ID )
Knowlege = 0.0
Pop <- setKnowlege( Pop = Pop, K = Knowlege )
Pop <- updateLearningRate_Knowlege( Pop = Pop )
SubPop <- sel_rnd_SubPop(Pop = Pop, n = 1)$sel
SubPop <- setKnowlege(Pop = SubPop, K = 1)
Pop <- integrate_SubPop(Pop = Pop, SubPop = SubPop)
Pop <- updateLearningRate_Knowlege( Pop = Pop )
Pop <- setStudyTime( Pop = Pop )
Pop

A <- sim_rnd(Pop = Pop, STn = 8, n = 2000, LR = 0.1)

A$Pop
A$TL


ggplot(data = A$TL, aes(x = Time, y = Knowlege, group = ID)) +
  geom_line(color = "grey", alpha = 0.5) +  # Linien für IDs
  geom_line(data = A$TL, aes(x = Time, y = Mean), color = "black") +  # Schwarze Mittelwertlinie
  ggtitle("Timeline Mean Knowlege") +
  xlab("Number of exchanges") +
  ylab("Knowledge") +
  scale_y_continuous(
    limits = c(0, 1),         # Y-Achse auf 0 bis 1 begrenzen
    breaks = seq(0, 1, 0.1)   # Schritte von 0.1 festlegen
  ) +theme_light() +
  theme(
    legend.position = c(0.95, 0.05),       # Legende unten rechts (x = 0.5, y = 0.05)
    legend.justification = c(1, 0),       # Legende relativ zur Position ausrichten
    legend.background = element_rect(fill = "white", color = "black"), # Hintergrund für bessere Lesbarkeit
    legend.title = element_blank()        # Entfernt den Legendentitel (optional)
  )

