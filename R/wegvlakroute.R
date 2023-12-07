##### ReadNLwegvlakken
# libs
library(dtplyr)

# Read CSV:
wegvlakken <- read.csv("data/NLRectwegvlakken2.csv")
#with attr: 
# "X" - skip
# "new_ID" - new ID of uniform part of wegvlag
# "radius" - if not straight
# "X_center" 
#"Y_center"
#"G_type"
#"Azimuth" degrees vs North
#"Length"  of the new, uniform part of wegvlag
#"WVK_ID" ID of original wegvlak
#"WEGBEHSRT"
#"RIJRICHTNG" B = beide, H = 1 richting
#"WEG_CAT"    
#"AUTO_H"
#"AUTO_T"
#"MAXSHD" maximum speed
#"MAXSHD_ALT"             
#"MAXSHD_ADV" advised speed
#"Road_Type"
#"x_start" RD coordinates
#"x_end"                  
#"y_end"
#"y_start"
#"Surface"
#"Int_Light_Day"          
#"Int_Light_Eve"
#"Int_Light_Night"
#"Intensity_Light_Daily"
#"Intensity_Light_Yearly" 
#"Int_Medium_Day"
#"Int_Medium_Eve"
#"Int_Medium_Night"
#"Intensity_Medium_Daily" 
#"Intensity_Medium_Yearly"
#"Int_Heavy_Day"
#"Int_Heavy_Eve"
#"Int_Heavy_Night"        
#"Intensity_Heavy_Daily"
#"Intensity_Heavy_Yearly"
#"BorderID1"
#"BorderID2"              
#"BorderID3"
#"BorderID4"
#"BorderID5"
#"BorderID6"              
#"Borderspeed1"
#"Borderspeed2"
#"Borderspeed3"
#"Borderspeed4"           
#"Borderspeed5"            
#"Borderspeed6"     


# loop over the wegvlakken version 1: start ... TODO
# sort by 1. Wegvlak  2. new_ID 3. connected via BorderID1 ???
# find unique points begin or end of wegvlak = aggragate by min of new_ID
Beginnings <- match(unique(wegvlakken$WVK_ID), wegvlakken$WVK_ID)
revEndings <- match(unique(wegvlakken$WVK_ID), rev(wegvlakken$WVK_ID))
Endings <- nrow(wegvlakken) - revEndings
#select one way streets, and their headings
OneWayBegin <- wegvlakken[Beginnings, c("WVK_ID", "x_start", "y_start", "RIJRICHTNG", "Azimuth")]
OneWayBegin <- OneWayBegin[OneWayBegin$RIJRICHTNG == "H",]
OneWayEnd <- wegvlakken[Endings, c("WVK_ID", "x_end", "y_end", "RIJRICHTNG")]
OneWayEnd <- OneWayEnd[OneWayEnd$RIJRICHTNG == "H",]
OneWay <- merge(OneWayBegin, OneWayEnd)
OneWay$heading <- atan2(OneWay$y_end - OneWay$y_start, OneWay$x_end - OneWay$x_start) / pi * 180
OneWay$heading[OneWay$heading < 0] <- 360 + OneWay$heading[OneWay$heading < 0]
OneWay$ddir <- abs(OneWay$heading - OneWay$Azimuth)
plot(ecdf(OneWay$ddir))
cat ("no way One way")

# 
wegvlakken$travelweight <- wegvlakken$Length / wegvlakken$MAXSHD
aggPweg <- aggregate(travelweight ~ WVK_ID, data = wegvlakken, FUN = sum)

##### make unique vertices
BeginningCross <- wegvlakken[Beginnings, c("WVK_ID", "new_ID", "x_start", "y_start")]
#round to integers, meters should be acurate enough, faster comparison
BeginningCross$x <- as.integer(BeginningCross$x_start)
BeginningCross$y <- as.integer(BeginningCross$y_start)
EndCross <- wegvlakken[Endings, c("WVK_ID", "new_ID", "x_end", "y_end")]
EndCross$x <- as.integer(EndCross$x_end)
EndCross$y <- as.integer(EndCross$y_end)
AllVertices <- unique(rbind(BeginningCross[, c("x", "y")],
                            EndCross[, c("x", "y")]))
AllVertices$vertID <- 1:nrow(AllVertices)
#join this vertID back to aggPweg to obtain the edges
Pweg1 <- merge(aggPweg, BeginningCross[,c("WVK_ID", "x", "y")])
Pweg1a <- merge(Pweg1, AllVertices)
#clean and prep for endpoint: loose x, y vertID is for startpoint
Pweg1a$x <- NULL;  Pweg1a$y <- NULL
names(Pweg1a) <- c("WVK_ID","travelweight","vertIDbegin")
Pweg2 <- merge(Pweg1a, EndCross[,c("WVK_ID", "x", "y")])
Pweg2a <- merge(Pweg2, AllVertices)
Pweg2a$x <- NULL;  Pweg2a$y <- NULL
names(Pweg2a) <- c("WVK_ID","weight","vertIDbegin", "vertIDend")
#N.B. hardcoded "weight" name is used in shortest_path
library(igraph)
netw <- graph_from_data_frame(Pweg2a[,c("vertIDbegin", "vertIDend", "WVK_ID", "weight")],
                              directed = F)
shortest_paths(netw, from = "537", to = V(netw)["221"])
