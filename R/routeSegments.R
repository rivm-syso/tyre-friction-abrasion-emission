library(sf)

# Define opposite / parallel threshold
opposite_threshold <- 0.02 # Rad




#need startendpoints for crossing
startPnt <- readRDS("data/startPnt.RDS")
endPnt <- readRDS("data/endPnt.RDS")

sPIDdf <- as.data.frame(round(st_coordinates(startPnt)))
startPntsPID <- paste(round(sPIDdf$X), round(sPIDdf$Y), sep = "")
sPIDdf <- as.data.frame(round(st_coordinates(endPnt)))
endPntsPID <- paste(round(sPIDdf$X), round(sPIDdf$Y), sep = "")

connections <- as.data.frame(table(c(startPntsPID, endPntsPID)))

listRoutes <- readRDS("data/listRoutes.RDS")
listRoutes <- listRoutes[!sapply(listRoutes,is.null)]
WegVKManeuvers <- readRDS("/rivm/r/E121554 LEON-T/03 - uitvoering WP3/Stage Anne/Output data/Road_Network_with_maneuvers.rds")
Utrecht_veluwe_IDs <- read.csv("data/Utrecht_veluwe_IDs.csv")
WegVKManeuvers$Heading <- atan2(WegVKManeuvers$y_end - WegVKManeuvers$y_start, WegVKManeuvers$x_end - WegVKManeuvers$x_start) 
WegVKManeuvers$Heading[WegVKManeuvers$Heading < 0] <- WegVKManeuvers$Heading[WegVKManeuvers$Heading < 0] + 2 * pi
WegVKManeuversUtrVel <- WegVKManeuvers[WegVKManeuvers$WVK_ID %in% Utrecht_veluwe_IDs$WVK_ID,
                                       c("WVK_ID", "Length", "WEG_CAT", "Heading", "MAXSHD", "Surface", 
                                         "x_start", "x_end", "y_start", "y_end", "Type", "Dist_start", "Dist_end")]

#rm(WegVKManeuvers)
are_almost_parallel <- function(slope1, slope2, opposite_threshold) {
  if (is.na(slope1) | is.na(slope2)) return (F)
  if (is.infinite(slope1) | is.infinite(slope2)) return(F)
  (abs(abs(slope1 - slope2) - pi) < opposite_threshold) |
    (abs(slope1 - slope2) < opposite_threshold)
}

calcSlope <- function(y1, y2, x1, x2){
  ret <- atan2(y2 - y1, x2 - x1)
  if (ret < 0) ret <- ret + 2 * pi
  return(ret)
}

routeTest <- listRoutes[[1]]
rTasList <- lapply (1:nrow(routeTest), function(i) {#i = 4
  WegVManeuvres <- WegVKManeuversUtrVel[WegVKManeuversUtrVel$WVK_ID == routeTest[i,1],]
  WegVKHeadi <- WegVKHeadings[WegVKHeadings$WVK_ID ==  routeTest[i,1],]
  Coords <- st_coordinates(WegVKHeadi)
  segmInfo <- segmInfo[F,]
  perow <- lapply(2:nrow(Coords), function(j){
    segm1 <- Coords[j-1,]
    segm2 <- Coords[j,]
    heading <- calcSlope(segm2[2],  segm1[2], segm2[1], segm1[1])

    dx <- segm2[1] - segm1[1]
    dy <- segm2[2] - segm1[2]
    lengt <- sqrt(dx*dx + dy*dy)
    data.frame(seqno = j, Lengt = lengt, Heading = heading)
  }) 
  
  if (i > 1){
    #possibly insert corner / crossing
    PrevManeuvre <-  WegVKManeuversUtrVel[WegVKManeuversUtrVel$WVK_ID == routeTest[i-1,1],]
    if(routeTest[i-1,2]==1) {#start to end
      prevsegment <- PrevManeuvre[1,]
      Junctionpnt <- paste(round(prevsegment$x_start), round(PrevManeuvre$y_start), sep = "")
    } else {
      prevsegment <- PrevManeuvre[nrow(PrevManeuvre),] #last! travelling end to start
      Junctionpnt <- paste(round(prevsegment$x_end), round(PrevManeuvre$y_end), sep = "")
    }
    #find cross in all junctions
    junction <- connections$Freq[connections$Var1 == Junctionpnt]
    if (junction > 2) { # a crossing
      if (routeTest[i-1,2]==1) {#start to end
        fromHeading <- calcSlope(prevsegment$y_end, prevsegment$y_start, prevsegment$x_end, prevsegment$x_start)
      } else {
        fromHeading <- calcSlope(prevsegment$y_start, prevsegment$y_end, prevsegment$x_start, prevsegment$x_end)
      }
      if (routeTest[i,2]==1) {#start to end
        toHeading <- calcSlope(prevsegment$y_end, prevsegment$y_start, prevsegment$x_end, prevsegment$x_start)
      } else {
        toHeading <- calcSlope(prevsegment$y_start, prevsegment$y_end, prevsegment$x_start, prevsegment$x_end)
      }
      toCorner <- abs(toHeading - fromHeading) # will be Heading !Abuse of column
      if ((toCorner > pi / 6 & toCorner < pi / 3) | 
          (toCorner > 5 * pi / 6 & toCorner < 2 * pi / 3) ) {# my guess
        Cornerdf <- data.frame(
          Length = 0,
          WVK_ID = WegVManeuvres$WVK_ID[1],
          MAXSHD = WegVManeuvres$MAXSHD[1],
          WEG_CAT = WegVManeuvres$WEG_CAT[1],
          Surface = WegVManeuvres$Surface[1],
          Heading = toCorner,
          Type = "TakeTurn",
          Dist_start = 0,
          Dist_end = 0
        )
      } else { 
        Cornerdf <- data.frame(
          Length = 0,
          WVK_ID = WegVManeuvres$WVK_ID[1],
          MAXSHD = WegVManeuvres$MAXSHD[1],
          WEG_CAT = WegVManeuvres$WEG_CAT[1],
          Surface = WegVManeuvres$Surface[1],
          Heading = toCorner,
          Type = "Crossing",
          Dist_start = 0,
          Dist_end = 0
        )
      }
      
    } else {#empty
    Cornerdf <- NULL
    }
  }
  inDF <- do.call(rbind, perow)
  inDF$Length <- cumsum(inDF$Lengt)
  inDF$WVK_ID <- WegVManeuvres$WVK_ID[1]
  inDF$MAXSHD <- WegVManeuvres$MAXSHD[1]
  inDF$WEG_CAT <- WegVManeuvres$WEG_CAT[1]
  inDF$Surface <- WegVManeuvres$Surface[1]
  inDF$Type <- "Corner"
  inDF$Dist_start <- inDF$Length
  inDF$Dist_end <- st_length(WegVKHeadi)
  
  res <- rbind(WegVManeuvres[!names(WegVManeuvres) %in% c( "x_start","x_end","y_start","y_end")], 
               Cornerdf,
               inDF[,names(WegVManeuvres)[!names(WegVManeuvres) %in% c( "x_start","x_end","y_start","y_end")]])
  if(routeTest[i,2]==1) {
    return(res[order(res$Dist_start, decreasing = F),])
  } else {
    return(res[order(res$Dist_start, decreasing = T),])
  }
  
})

EenRoute <- do.call(rbind, rTasList)
