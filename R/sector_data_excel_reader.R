## library set up ##
library (readxl)
library(dplyr)

## Open Excel in R ##
maneuver_data <- read_excel("R/exercise_excel_2.xlsx")
maneuver_data <- data.frame(maneuver_data)

maneuver_data$c_accel
rowwise(maneuver_data)
maneuver_data%>% mutate(min_long_force = min(f_accel_long_force(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel = Start.speed, v_end_accel=Top.speed, v_wind, alpha_slope, m_rotate, c_accel=c_accel)))





# Add minimum longitudinal force as column to maneuver_data


                                     










#First_column <- select(sector_data, c('First column'))
#Second_column <- select(sector_data, c('Second column'))
#Third_column <- First_column * Second_column

