library(dplyr)
const_speed_20_kph_data <- AllData_fw |> filter(`Maneuver number`== "MUcs20kph1")
const_speed_20_kph_data <- const_speed_20_kph_data |> mutate("FW per m" = list(FWConstLong / DistanceConst))
