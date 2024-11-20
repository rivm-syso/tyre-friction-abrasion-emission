source("R/ReadIDIADAtracksAs1.R")

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

