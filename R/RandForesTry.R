source("R/ReadIDIADAtracksAs1.R")
library(randomForest)
library(caret)
library(pdp)
#once to sum easy
#for (dfName in names(TrackSumData)) {
#  df <- TrackSumData[[dfName]]
#  sumDist <- sum(df$distance)
#  df$DistFrac = df$distance / sumDist
#  TrackSumData[[dfName]] <- df
#}
names(general)
stopifnot(!all(rownames(perkmTrack) == general$TrackName))

general <- cbind(general, perkmTrack)
DFin <- merge(WearAsLong, general[, c("TrackType", "decel_ms2", "accel_ms2", "accelFrac", "decelFrac",
              "mainSpeed", "mainCorner1dRadius", "maincornerSpeed", "CornerFraction", "breaks",
              "distance","rotationpkm", "AIname", "centrW_g_1",
              "decelW_g_1","accelW_g_1","airRes_Ac_1")], 
              by.x = "track", by.y = "AIname")

DFin$DecelJpKM <- ifelse(DFin$Tyre == "LingLongHighload", heavy_veh, m_vehicle) * DFin$decelW_g_1
DFin$AccelJpKM <- ifelse(DFin$Tyre == "LingLongHighload", heavy_veh, m_vehicle) * DFin$accelW_g_1
DFin$centrJpKM <- ifelse(DFin$Tyre == "LingLongHighload", heavy_veh, m_vehicle) * DFin$centrW_g_1


#remove ??
#DFin$track <- NULL
#DFin$mainSpeed <- NULL
#DFin$maincornerSpeed <- NULL
#DFin$mainCorner1dRadius <- NULL
#DFin$TrackType <- NULL
#DFin$Wheel <- NULL

#rf <- randomForest(wear~., data = DFin)
#varImpPlot(rf, main = "wear [mg/km] variable importance" )
#ppTyre <- as.data.frame(partialPlot(rf, pred.data = DFin, x.var = "Tyre", ))
#library(ggplot2)
#ggplot(data = ppTyre, aes(x = x, y = y)) + geom_bar(stat = "identity")
#ppWheel <- as.data.frame(partialPlot(rf, pred.data = DFin, x.var = "FrOrRear", ))
#ggplot(data = ppWheel, aes(x = x, y = y)) + geom_bar(stat = "identity")
#ppRotPW <- as.data.frame(partialPlot(rf, pred.data = DFin, x.var = "rotationpkm", ))

#ppWlTT <- partial(rf, pred.var = "Tyre") 
#plot(ppWlTT)

rf <- randomForest(wear~ centrW_g_1+decelW_g_1+accelW_g_1+airRes_Ac_1, data = DFin)
rf <- randomForest(wear~ centrW_g_1+decelW_g_1+accelW_g_1+airRes_Ac_1+distance+TrackType, data = DFin)
rf <- randomForest(wear~ centrW_g_1+decelW_g_1+accelW_g_1+airRes_Ac_1+distance+TrackType+mainSpeed, data = DFin)
rf <- randomForest(wear~ centrW_g_1+decelW_g_1+accelW_g_1+airRes_Ac_1+distance+TrackType+mainSpeed+Tyre, data = DFin)

rf <- randomForest(wear~ centrW_g_1+decelW_g_1+accelW_g_1+ownWind_Ac_1+distance+TrackType+mainSpeed+FrOrRear, data = DFin)
varImpPlot(rf, main = "wear [mg/km] variable importance" )
rf <- randomForest(wear~ centrW_g_1+decelW_g_1+accelW_g_1+ownWind_Ac_1+distance+TrackType+mainSpeed+FrOrRear+Tyre, data = DFin)


DFinAgg <- aggregate(wear~ Tyre + track, data = WearAsLong, FUN = mean)
DFinAgg <- merge(DFinAgg, general[, c("TrackType", "decel_ms2", "accel_ms2", "accelFrac", "decelFrac",
                              "mainSpeed", "mainCorner1dRadius", "maincornerSpeed", "CornerFraction", "breaks",
                              "distance","rotationpkm", "AIname", "centrW_g_1",
                              "decelW_g_1","accelW_g_1","airRes_Ac_1")], 
      by.x = "track", by.y = "AIname")

rf <- randomForest(wear~ centrW_g_1+decelW_g_1+accelW_g_1+ownWind_Ac_1+distance+TrackType+mainSpeed+Tyre, data = DFinAgg)




library(mgcv)
gmod <- gam(data = DFin, formula = wear~ centrW_g_1+decelW_g_1+accelW_g_1+ownWind_Ac_1+distance+TrackType+mainSpeed+FrOrRear+Tyre)
gmod <- gam(data = DFinAgg, formula = wear~ centrW_g_1+decelW_g_1+accelW_g_1+ownWind_Ac_1+distance+TrackType+mainSpeed+Tyre)

summary(gmod)
