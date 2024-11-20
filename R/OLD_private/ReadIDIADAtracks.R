##### Read Sectors from IDIADA tracks
# is from excel 3 columns:  SECTOR | variable, Value, Unit
library(openxlsx)
source("R/dv_distance.R")
TrackFilename <- "/rivm/r/E121554 LEON-T/03 - uitvoering WP3/IDIADA track data/IDIDAsTables.xlsx"
Tracks <- openxlsx::getSheetNames(TrackFilename)
#First separate General and read them all
general <- openxlsx::read.xlsx(xlsxFile = TrackFilename, sheet = "general")
TracksData <- list()
TrackSumData <- list()
for (Track in Tracks[Tracks != "general"]) {#Track = "rural5"
  TracksData[[Track]] <- openxlsx::read.xlsx(TrackFilename, sheet = Track)
  #km/h to m/s conversion
  TracksData[[Track]]$velocity <- TracksData[[Track]]$velocity_kmh * 1000 / 3600
  TracksData[[Track]]$start_velocity <- TracksData[[Track]]$start_velocity_kmh * 1000 / 3600
}
#TracksData
finalNames <- c("distance", "velocity", "start_velocity", "corner_radius" )
for (TrackName in names(TracksData)) {#TrackName = "rural6"
  Track <- TracksData[[TrackName]]
  #remove testTot, if needed
  if ("testTot" %in% names(Track)) {
    Track <- Track[,names(Track)[names(Track)!="testTot"]]
  }
  #1 expand sectionRepeat, if needed
  if ("sectionRepeat" %in% names(Track)) {
    Track$sectionRepeat[is.na(Track$sectionRepeat)] <- 1
    SepRows <- lapply(1:nrow(Track), function(numline){ #numline = 1
     cbind(as.data.frame(lapply(Track[numline,names(Track)[names(Track)!="sectionRepeat"]], rep, Track[numline,"sectionRepeat"]))) #
    })
    Track <- do.call(rbind,SepRows)
  }
  #2 repeat df, re-bind 
  #Add0s <- T # add section to start and end at v = 0
  TotTrack <- cbind(as.data.frame(lapply(Track, rep, general$repeats[general$TrackName == TrackName]))) #
  # A exception 5*6 = 27 corrected in the last four rows for "rural6"
  if (TrackName == "rural6"){
    Aggr4 <- aggregate(distance ~ corner_radius + velocity , data = tail(TotTrack, 3), FUN = sum)
    Aggr4$start_velocity = Aggr4$velocity
    TotTrack <- rbind(head(TotTrack[, finalNames], nrow(TotTrack) - 3),
                      Aggr4[, finalNames])
  } else {
    TotTrack <- TotTrack[,finalNames]
  }
  #3 acceleration decel distances; begin and end at 0
  accellDist <- dv_distance(c(0,TotTrack$start_velocity), 
                            c(TotTrack$start_velocity[1], TotTrack$velocity), 
                            general$accel_ms2[general$TrackName == TrackName])
  StartNext <- c(TotTrack$start_velocity[-1], 0)
  descelDist <-  dv_distance(c(TotTrack$velocity), 
                             StartNext, 
                             - general$decel_ms2[general$TrackName == TrackName])
  #correct and split for ascel distances
  nwAscelRows <- which(accellDist[-1] > 0)
  if (length(nwAscelRows) > 0) {
    AscelDf <- data.frame(
      distance = accellDist[-1][nwAscelRows],
      velocity = (TotTrack$velocity[nwAscelRows] + TotTrack$start_velocity[nwAscelRows]) / 2, #mean
      centrfac = 0, # no ascel in corners
      accelleration = general$accel_ms2[general$TrackName == TrackName]
    )
    TotTrack$distance[nwAscelRows] <- TotTrack$distance[nwAscelRows] - AscelDf$distance
  } else AscelDf <- NULL
  nwDecelRows <-  which(descelDist > 0)
  #the last is not in TotTrack, so
  nwDecelRows <- nwDecelRows[nwDecelRows != nrow(TotTrack)]
  if (length(nwDecelRows) > 0) {
    DecelDf <- data.frame(
      distance = descelDist[nwDecelRows],
      velocity = (TotTrack$velocity[nwDecelRows] + TotTrack$start_velocity[nwDecelRows + 1]) / 2, #mean
      centrfac = 0, # no descel in corners
      accelleration = -general$decel_ms2[general$TrackName == TrackName]
    )
    TotTrack$distance[nwDecelRows] <- TotTrack$distance[nwDecelRows] - DecelDf$distance
  } else DecelDf <- NULL
  TotTrack$centrfac <- ifelse(TotTrack$corner_radius == 0, 0, TotTrack$velocity^2 / TotTrack$corner_radius)
  TotTrack$accelleration <- 0
  SectTrack <- rbind(
    TotTrack[,names(AscelDf)], AscelDf, DecelDf
  )
  #check!
  # :) sum(SectTrack$distance)
  
  row1 <- data.frame (
    distance = accellDist[1],
    velocity = TotTrack$velocity[1] / 2, #mean
    centrfac = 0,
    accelleration = general$accel_ms2[general$TrackName == TrackName]
  )
  lastrow <- data.frame (
    distance = tail(descelDist,1),
    velocity = tail(TotTrack$velocity, 1) / 2, #mean
    centrfac = 0,
    accelleration = -general$decel_ms2[general$TrackName == TrackName]
  )
  SectTrack <- rbind(
    row1,
    SectTrack,
    lastrow
  )
  SumTrack <- aggregate(distance ~ velocity + centrfac + accelleration, 
                        data = SectTrack,
                        FUN = sum)
  SumTrack$Track <- TrackName
  TrackSumData[[TrackName]] <- SumTrack
}
TrackSum <- do.call(rbind, TrackSumData)
DistSum <- aggregate(distance~Track, data = TrackSum, FUN = sum)

library(tidyr)
IDIADAwear <- read.xlsx("/rivm/r/E121554 LEON-T/03 - uitvoering WP3/IDIADA track data/Abrasion test_WP2.3_Leon-T_IDIADA.xlsx", startRow = 10)
#provide proper column names
names(IDIADAwear)[c(1,2)] <- c("Tyre","Wheel")
names(IDIADAwear) <- sapply(names(IDIADAwear), function(x){
  gsub("-", "_", x)
})
#extend Tyre to all its rows
for(i in 1:nrow(IDIADAwear)){
  if(is.na(IDIADAwear$Tyre[i])){
    IDIADAwear$Tyre[i] <- IDIADAwear$Tyre[i-1]
  }
}
#What?
IDIADAwear$C0 <- NULL
#Track to long format
WearAsLong <- tidyr::pivot_longer(IDIADAwear, 
                           names(IDIADAwear)[!names(IDIADAwear) %in% c("Tyre","Wheel")],
                           names_to = "track",
                           values_to = "wear")
WearAsLong$track[WearAsLong$track %in% c("R1", "R2", "R3")] <- "R"

#remove the aggregates 
WearAsLong <- WearAsLong[WearAsLong$Wheel %in% c("FR ", "FL", "RR ", "RL"),]
Map2track <- unique(WearAsLong$track)
table(WearAsLong$track)
general$TrackName
general$AIname <- c("T1_Ur","T2_Ur","T3_Ur","T4_Ur","T1_Rur","T2_Rur","T3_Rur","T1_Mot", "R")

Wear2Compare <- merge(WearAsLong, general[,c("AIname", "TrackName")], by.x = "track", by.y = "AIname")
Wheels2Compare <- aggregate(wear~TrackName+Wheel, data = Wear2Compare, FUN = mean)
Wheels2Compare <- merge(Wheels2Compare, DistSum)
Wheels2Compare$wear <- Wheels2Compare$wear * Wheels2Compare$distance
NormFacVel = 0.01

pars <- c(pvel = 0.1, Fpcentr = 1, Fpacc = 1, 
          RIpcentr = 1, ROpcentr = 1, 
          Rpacc = 0.1)
TheModel <- function(pars) {
  #AllWheels 
  Fvel <- pars["pvel"] * TrackSum$distance 
  #FrontWheels
  Fcentr <- pars["Fpcentr"] * abs(TrackSum$centrfac) * TrackSum$distance
  Fpacc <- pars["Fpacc"] * abs(TrackSum$accelleration) * TrackSum$distance
  #BothRear
  Rpacc <- pars["Rpacc"] * abs(TrackSum$accelleration) * TrackSum$distance
  #RearOuter
  ROcentr <- pars["ROpcentr"] * abs(TrackSum$centrfac) * TrackSum$distance
  #RearInner
  RIcentr <- pars["RIpcentr"] * abs(TrackSum$centrfac) * TrackSum$distance

  #aggregate per wheel
  FrontW <- Fvel + Fcentr + Fpacc
  #Flip InnerOuter to LR
  RR <- Fvel + Rpacc + ifelse(TrackSum$centrfac == 0, 0,
                              ifelse(TrackSum$centrfac > 0, #right turn 
                                     RIcentr, ROcentr)
  )
  RL <- Fvel + Rpacc + ifelse(TrackSum$centrfac == 0, 0,
                              ifelse(TrackSum$centrfac > 0, #right turn 
                                     ROcentr, RIcentr)
  )
  #sum per track
  WearResF <- aggregate(FrontW, list(TrackSum$Track), FUN = sum)
  WearResRR <- aggregate(RR, list(TrackSum$Track), FUN = sum)
  WearResRL <- aggregate(RL, list(TrackSum$Track), FUN = sum)
  
  #Diff 2be minimised
  MatchWheelFL <- match(WearResF$Group.1, Wheels2Compare$TrackName[Wheels2Compare$Wheel == "FL"])
  FLoff <- sum(abs(WearResF$x[MatchWheelFL] - Wheels2Compare$wear[Wheels2Compare$Wheel == "FL"]))
  MatchWheelRL <- match(WearResF$Group.1, Wheels2Compare$TrackName[Wheels2Compare$Wheel == "RL"])
  RLoff <- sum(abs(WearResF$x[MatchWheelRL] - Wheels2Compare$wear[Wheels2Compare$Wheel == "RL"]))
  MatchWheelRR <- match(WearResF$Group.1, Wheels2Compare$TrackName[Wheels2Compare$Wheel == "RR"])
  RRoff <- sum(abs(WearResF$x[MatchWheelRR] - Wheels2Compare$wear[Wheels2Compare$Wheel == "RR"]))
  
  return(FLoff + RLoff + RRoff)
}

optpars <- optim(par = pars, fn = TheModel, method = "L-BFGS-B")
optpars

library(mgcv)
gmod <- gam(data = Wear2Compare[Wear2Compare$track == "R",], formula = wear ~ Tyre + Wheel)
summary(gmod)

