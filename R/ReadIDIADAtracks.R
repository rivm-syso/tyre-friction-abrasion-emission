##### Read Sectors from IDIADA tracks
# is from excel 3 columns:  SECTOR | variable, Value, Unit
library(openxlsx)

Tracks <- openxlsx::getSheetNames()
TracksData <- list()
for (Track in Tracks) {#Track = Tracks[1]
  TracksData[[Track]] <- openxlsx::read.xlsx()
}