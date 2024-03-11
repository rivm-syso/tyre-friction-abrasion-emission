## library set up ##
library (readxl)
library(dplyr)

## Open Excel in R ##
sector_data <- read_excel("R/exercise_excel.xlsx")

## Select columns from sector_data ##

First_column <- select(sector_data, c('First column'))
Second_column <- select(sector_data, c('Second column'))
Third_column <- First_column * Second_column

