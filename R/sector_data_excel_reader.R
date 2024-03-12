## library set up ##
library (readxl)
library(dplyr)

## Open Excel in R ##
maneuver_data <- read_excel("R/exercise_excel_2.xlsx")

## Select columns from sector_data ##

v_start <- select(maneuver_data, c ('Start speed')) 
v_end <- select (maneuver_data, c ('End speed'))
v_top <- select (maneuver_data, c ('Top speed'))
alpha_slope <- atan(select(maneuver_data, c('Slope')))
corner_angle <- select(maneuver_data, c ('Corner angle'))
r_corner <- select(maneuver_data, c ('Corner radius')) 
c_accel <- select(maneuver_data, c ('c_accel'))
c_decel <- select(maneuver_data, c ('c_decel'))
maneuver_repeats <- select(maneuver_data, c ('Maneuver repeats'))






#First_column <- select(sector_data, c('First column'))
#Second_column <- select(sector_data, c('Second column'))
#Third_column <- First_column * Second_column

