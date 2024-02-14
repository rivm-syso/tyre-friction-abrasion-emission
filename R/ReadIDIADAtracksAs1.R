#Calculate all to Joule / km
#step 1: one round to fractions 

#Basic forces
# AirResist <- function (c_drag,
#           A_vehicle,
#           rho_air,
#           velocity_kmh
#           # v_wind #no data; circuit goes 360.. neglected
#         ) {
#   pmax(0, c_drag * A_vehicle * rho_air * velocity_kmh^2)
# }

# rollF <- function(c_roll, m_vehicle, grav_constant) {
#     c_roll * m_vehicle * grav_constant
# }

#
#centripet_J_g_1 <- function()

#inert_force <- function(m_vehicle, m_rotate, c_accel) {
#    (m_vehicle + m_rotate) * c_accel
#}

#ThisData
m_vehicle = 1644
heavy_veh = 1868
#Vehicle surface area in m^2
A_vehicle = 2.629
#Vehicle aerodynamic drag coefficient
c_drag = 0.347
### Gravitational constant in m.s^-2
grav_constant = 9.81
## Landscape data ##
###Density of air in kg.m^-3
rho_air = 1.205


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
for (TrackName in names(TracksData)) {#TrackName = "urban2"
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
      start_velocity = TotTrack$start_velocity[nwAscelRows],
      corner_radius = TotTrack$corner_radius[nwAscelRows],
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
      start_velocity = TotTrack$start_velocity[nwDecelRows],
      corner_radius = TotTrack$corner_radius[nwDecelRows],
      centrfac = 0, # no descel in corners
      accelleration = -general$decel_ms2[general$TrackName == TrackName]
    )
    TotTrack$distance[nwDecelRows] <- TotTrack$distance[nwDecelRows] - DecelDf$distance
  } else DecelDf <- NULL
  TotTrack$centrfac <- ifelse(TotTrack$corner_radius == 0, 0, TotTrack$velocity^2 / TotTrack$corner_radius)
  TotTrack$accelleration <- 0
  SectTrack <- rbind(
    TotTrack[,c(finalNames, "accelleration", "centrfac")], 
    AscelDf[,c(finalNames, "accelleration", "centrfac")], 
    DecelDf[,c(finalNames, "accelleration", "centrfac")]
  )
  #check!
  # :) sum(SectTrack$distance)
  
  row1 <- data.frame (
    distance = accellDist[1],
    velocity = TotTrack$velocity[1] / 2, #mean
    start_velocity = 0,
    centrfac = 0,
    corner_radius = 0,
    accelleration = general$accel_ms2[general$TrackName == TrackName]
  )
  lastrow <- data.frame (
    distance = tail(descelDist,1),
    velocity = tail(TotTrack$velocity, 1) / 2, #mean
    start_velocity = tail(TotTrack$velocity, 1),
    corner_radius = 0,
    centrfac = 0,
    accelleration = -general$decel_ms2[general$TrackName == TrackName]
  )
  SectTrack <- rbind(
    row1[,names(SectTrack)],
    SectTrack,
    lastrow[,names(SectTrack)]
  )
  SumTrack <- aggregate(distance ~ velocity + centrfac + accelleration, 
                        data = SectTrack,
                        FUN = sum)
  SumTrack$Track <- TrackName
  TrackSumData[[TrackName]] <- SumTrack
}
TrackSum <- do.call(rbind, TrackSumData)
DistSum <- aggregate(distance~Track, data = TrackSum, FUN = sum)

#from J + distance to J / km
perkmTrack <- list()
for (TrackName in names(TrackSumData)){#TrackName = "RunIn" names(TrackSumData)[2]
  TRfr <- TrackSumData[[TrackName]]
  TRfr$duration <- TRfr$distance / TRfr$velocity
  TotDistKm <- DistSum$distance[DistSum$Track == TrackName] 
  TRfr$centrW_g_1 <- abs(TRfr$centrfac) * TRfr$duration / TotDistKm
  TRfr$decelW_g_1 <- -min(0, TRfr$accelleration) * TRfr$duration / TotDistKm
  TRfr$accelW_g_1 <- max(0, TRfr$accelleration) * TRfr$duration / TotDistKm
  TRfr$ownWind_Ac_1 <- TRfr$velocity^2  / TotDistKm
  perkmTrack[[TrackName]] <- c(
                  centrW_g_1 = sum(TRfr$centrW_g_1),
                  decelW_g_1 = sum(TRfr$decelW_g_1),
                  accelW_g_1 = sum(TRfr$accelW_g_1),
                  ownWind_Ac_1 = sum(TRfr$ownWind_Ac_1)
                  )
}
perkmTrack <- as.data.frame(do.call(rbind, perkmTrack))

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
                           cols = names(IDIADAwear)[!names(IDIADAwear) %in% c("Tyre","Wheel")], #,"Wheel"
                           names_to = "track",
                           values_to = "wear")
WearAsLong$Wheel <- trimws(WearAsLong$Wheel)
WearAsLong$Tyre <- gsub(" ", "", WearAsLong$Tyre)

WearWheelsWideAgain <- tidyr::pivot_wider(WearAsLong, names_from = Wheel, values_from = wear)
#Change Left/Right to Inner/Outer
WearWheelsWideAgain$FInner <- ifelse(endsWith(WearWheelsWideAgain$track, "_Ur"), 
                                     WearWheelsWideAgain$FL, WearWheelsWideAgain$FR)
WearWheelsWideAgain$FOuter <- ifelse(endsWith(WearWheelsWideAgain$track, "_Ur"), 
                                     WearWheelsWideAgain$FR, WearWheelsWideAgain$FL)
WearWheelsWideAgain$RInner <- ifelse(endsWith(WearWheelsWideAgain$track, "_Ur"), 
                                     WearWheelsWideAgain$RL, WearWheelsWideAgain$RR)
WearWheelsWideAgain$ROuter <- ifelse(endsWith(WearWheelsWideAgain$track, "_Ur"), 
                                     WearWheelsWideAgain$RR, WearWheelsWideAgain$RL)
WearWheelsWideAgain$FrontLRatio <- WearWheelsWideAgain$FL / WearWheelsWideAgain$FR
WearWheelsWideAgain$RearLRatio <- WearWheelsWideAgain$RL / WearWheelsWideAgain$RR
#backtolong #1 for plot
WearRLLong <- tidyr::pivot_longer(data = WearWheelsWideAgain, 
                                  cols = c("FrontLRatio", "RearLRatio"), #,"Wheel"
                                  names_to = "LRatio",
                                  values_to = "wear")
ggplot(data = WearRLLong[WearRLLong$track %in% c("T1_Ur", "T2_Ur")], aes(x = LRatio, y = wear, fill = LRatio)) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.1) +
  facet_wrap(~track) +
  coord_cartesian(ylim = c(0,4))


#Back to long
WearAsLong <- tidyr::pivot_longer(data = WearWheelsWideAgain, 
                                  cols = c("FInner", "FOuter", "RInner", "ROuter"), #,"Wheel"
                                  names_to = "Wheel",
                                  values_to = "wear")
#correct for T1_Ur and T2_Ur which are 8 shaped
WearAsLong$Wheel[WearAsLong$track %in% c("T1_Ur", "T2_Ur") & startsWith(WearAsLong$Wheel, "F")] <- "Front"
WearAsLong$Wheel[WearAsLong$track %in% c("T1_Ur", "T2_Ur") & startsWith(WearAsLong$Wheel, "R")] <- "Rear"

#Potentially simplify
WearAsLong$FrOrRear <- startsWith(WearAsLong$Wheel, "F")

#Drop now superfluous columns 
WearAsLong <- WearAsLong[,c("Tyre", "track", "wear", "Wheel", "FrOrRear")]
Map2track <- unique(WearAsLong$track)
table(WearAsLong$track)

#strings to factor
WearAsLong$Tyre <- as.factor(WearAsLong$Tyre)
WearAsLong$Wheel <- as.factor(WearAsLong$Wheel)
#WearAsLong$track <- as.factor(WearAsLong$track)

#randomForest invest R1, R2, R3 trend?
library(randomForest)
rf <- randomForest(wear~ Tyre + track + FrOrRear,WearAsLong[WearAsLong$track %in% c("R1", "R2", "R3"),])
varImpPlot(rf, main = "wear [mg/km] trend R123" )
cat ("rsq for Runin wear ~ Tyre + track + FrOrRear")
mean(rf$rsq)
rf <- randomForest(wear~track,WearAsLong[WearAsLong$track %in% c("R1", "R2", "R3"),])
cat ("rsq for Runin wear ~ track")
mean(rf$rsq)

#Remove the first 2 RunIn 
WearAsLong <- WearAsLong[!WearAsLong$track %in% c("R1", "R2"),]
#WearAsLong$track[WearAsLong$track %in% c("R1", "R2", "R3")] <- "R"
general$TrackName
general$AIname <- c("T1_Ur","T2_Ur","T3_Ur","T4_Ur","T1_Rur","T2_Rur","T3_Rur","T1_Mot", "R3")

