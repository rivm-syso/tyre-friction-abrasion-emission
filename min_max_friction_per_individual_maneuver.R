library(dplyr)

# mutate AllData_fw to get friction work intensity in J per m

AllData_fw<- AllData_fw |> mutate ("FWAccellong_per_m" = list(FWAccelLong/DistanceManAccel)
                                          , "FWDecellong_per_m" =list(FWDecelLong/DistanceManDecel)
                                          ,"FWConstSpeed_per_m" = list(FWConstLong/DistanceConst))


# add minima and maxima to AllData_fw

AllData_fw <- AllData_fw |> mutate("Min FW Accellong per m" = min(FWAccellong_per_m)
                                  ,"Max FW Accellong per m" = max(FWAccellong_per_m),
                                  "Min FW Decellong per m" = min(FWDecellong_per_m)
                                  , "Max FW Decellong per m" = max(FWDecellong_per_m)
                                  , "Min FW Const speed per m" = min(FWConstSpeed_per_m)
                                  , "Max FW Const speed per m" = max(FWConstSpeed_per_m))




# filter Alldata_fw_per_m for different maneuvers and get minimum and maximum friction work intensity per maneuver

const_speed_20_kph_data <- AllData_fw |> filter(`Maneuver number`== "MUcs20kph1")
min_FW_per_m_const_speed_20_kph = min(const_speed_20_kph_data$`Min FW Const speed per m`)
max_FW_per_m_const_speed_20_kph = max(const_speed_20_kph_data$`Max FW Const speed per m`)

const_speed_30_kph_data <- AllData_fw |> filter(`Maneuver number`== "MUpa30kph1")
min_FW_per_m_const_speed_30_kph = min(const_speed_30_kph_data$`Min FW Const speed per m`)
max_FW_per_m_const_speed_30_kph = max(const_speed_30_kph_data$`Max FW Const speed per m`)

const_speed_40_kph_data <- AllData_fw |> filter(`Maneuver number`== "MUpa40kph1")
min_FW_per_m_const_speed_40_kph = min(const_speed_40_kph_data$`Min FW Const speed per m`)
max_FW_per_m_const_speed_40_kph = max(const_speed_40_kph_data$`Max FW Const speed per m`)

const_speed_60_kph_data <- AllData_fw |> filter(`Maneuver number`== "MRu60kph3")
min_FW_per_m_const_speed_60_kph = min(const_speed_60_kph_data$`Min FW Const speed per m`)
max_FW_per_m_const_speed_60_kph = max(const_speed_60_kph_data$`Max FW Const speed per m`)

const_speed_70_kph_data <- AllData_fw |> filter(`Maneuver number`== "MRu70kph3")
min_FW_per_m_const_speed_70_kph = min(const_speed_70_kph_data$`Min FW Const speed per m`)
max_FW_per_m_const_speed_70_kph = max(const_speed_70_kph_data$`Max FW Const speed per m`)

const_speed_80_kph_data <- AllData_fw |> filter(`Maneuver number`== "MRu70kph3")
min_FW_per_m_const_speed_80_kph = min(const_speed_80_kph_data$`Min FW Const speed per m`)
max_FW_per_m_const_speed_80_kph = max(const_speed_80_kph_data$`Max FW Const speed per m`)

const_speed_130_kph_data <- AllData_fw |> filter(`Maneuver number`== "MMoW3")
min_FW_per_m_const_speed_130_kph = min(const_speed_130_kph_data$`Min FW Const speed per m`)
max_FW_per_m_const_speed_130_kph = max(const_speed_130_kph_data$`Max FW Const speed per m`)

decel_20_0_kph_data <-AllData_fw |> filter(`Maneuver number`== "MUbr20kph1")
min_FW_per_m_decel_20_0_kph = min(decel_20_0_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_20_0_kph  = max(decel_20_0_kph_data$`Max FW Decellong per m`)

decel_30_0_kph_data <-AllData_fw |> filter(`Maneuver number`== "MUpa30kph12")
min_FW_per_m_decel_30_0_kph = min(decel_30_0_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_30_0_kph  = max(decel_30_0_kph_data$`Max FW Decellong per m`)

decel_40_0_kph_data <-AllData_fw |> filter(`Maneuver number`== "MUpa40kph12")
min_FW_per_m_decel_40_0_kph = min(decel_40_0_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_40_0_kph  = max(decel_40_0_kph_data$`Max FW Decellong per m`)

decel_30_20_kph_data <-AllData_fw |> filter(`Maneuver number`== "MUpa30kph3")
min_FW_per_m_decel_30_20_kph = min(decel_30_20_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_30_20_kph  = max(decel_30_20_kph_data$`Max FW Decellong per m`)

decel_40_20_kph_data <-AllData_fw |> filter(`Maneuver number`== "MUpa40kph3")
min_FW_per_m_decel_40_20_kph = min(decel_40_20_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_40_20_kph  = max(decel_40_20_kph_data$`Max FW Decellong per m`)

decel_60_50_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu60kph1")
min_FW_per_m_decel_60_50_kph = min(decel_60_50_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_60_50_kph  = max(decel_60_50_kph_data$`Max FW Decellong per m`)

decel_70_50_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu70kph1")
min_FW_per_m_decel_70_50_kph = min(decel_70_50_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_70_50_kph  = max(decel_70_50_kph_data$`Max FW Decellong per m`)

decel_80_50_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu80kph1")
min_FW_per_m_decel_80_50_kph = min(decel_80_50_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_80_50_kph  = max(decel_80_50_kph_data$`Max FW Decellong per m`)

decel_130_50_kph_data <-AllData_fw |> filter(`Maneuver number`== "MMoW1")
min_FW_per_m_decel_130_50_kph = min(decel_130_50_kph_data $`Min FW Decellong per m`)
max_FW_per_m_decel_130_50_kph  = max(decel_130_50_kph_data$`Max FW Decellong per m`)

accel_0_20_kph_data <-AllData_fw |> filter(`Maneuver number`== "MUbr20kph2")
min_FW_per_m_accel_0_20_kph = min(accel_0_20_kph_data $`Min FW Accellong per m`)
max_FW_per_m_accel_0_20_kph  = max(accel_0_20_kph_data$`Max FW Accellong per m`)

accel_50_70_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu70kph2")
min_FW_per_m_accel_50_70_kph = min(accel_50_70_kph_data $`Min FW Accellong per m`)
max_FW_per_m_accel_50_70_kph  = max(accel_50_70_kph_data$`Max FW Accellong per m`)

accel_50_80_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu80kph2")
min_FW_per_m_accel_50_80_kph = min(accel_50_80_kph_data $`Min FW Accellong per m`)
max_FW_per_m_accel_50_80_kph  = max(accel_50_80_kph_data$`Max FW Accellong per m`)

accel_50_130_kph_data <-AllData_fw |> filter(`Maneuver number`== "MMoW2")
min_FW_per_m_accel_50_130_kph = min(accel_50_130_kph_data $`Min FW Accellong per m`)
max_FW_per_m_accel_50_130_kph  = max(accel_50_130_kph_data$`Max FW Accellong per m`)

# including latitinal friction at corners
AllData_fw <- AllData_fw |> mutate("FW_corner_per_m" = list((FWLat*7+FWConstLong)/DistanceCorn))

AllData_fw <- AllData_fw |> mutate("Min FW corner per m" = min(FW_corner_per_m)
                                   ,"Max FW corner per m" = max(FW_corner_per_m))

corner_60_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu60kph4")
min_FW_per_m_corner_60_kph = min(corner_60_kph_data $'Min FW corner per m')
max_FW_per_m_corner_60_kph  = max(corner_60_kph_data$'Max FW corner per m')

corner_70_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu70kph4")
min_FW_per_m_corner_70_kph = min(corner_70_kph_data $'Min FW corner per m')
max_FW_per_m_corner_70_kph  = max(corner_70_kph_data$'Max FW corner per m')

corner_80_kph_data <-AllData_fw |> filter(`Maneuver number`== "MRu80kph4")
min_FW_per_m_corner_80_kph = min(corner_80_kph_data $'Min FW corner per m')
max_FW_per_m_corner_80_kph  = max(corner_80_kph_data$'Max FW corner per m')

corner_130_kph_data <-AllData_fw |> filter(`Maneuver number`== "MMoW4")
min_FW_per_m_corner_130_kph = min(corner_130_kph_data $'Min FW corner per m')
max_FW_per_m_corner_130_kph  = max(corner_130_kph_data$'Max FW corner per m')

corner_20_kph_data <-AllData_fw |> filter(`Maneuver number`== "MUcs20kph4")
min_FW_per_m_corner_20_kph = min(corner_20_kph_data $'Min FW corner per m')
max_FW_per_m_corner_20_kph  = max(corner_20_kph_data$'Max FW corner per m')

park_drive_away_data <-AllData_fw |> filter(`Maneuver number`== "MUpa30kph13")

park_drive_away_data <-park_drive_away_data |> mutate ("FW_accel_in_corner_per_m" = list((FWLat*7+FWAccelLong)/DistanceCorn))
park_drive_away_data <-park_drive_away_data |> mutate ("Min FW accel in corner per m" = min(FW_accel_in_corner_per_m)
                                                       , "Max FW accel in corner per m" = max(FW_accel_in_corner_per_m))

min_FW_per_m_park_drive_away = min(park_drive_away_data $'Min FW accel in corner per m')
max_FW_per_m_park_drive_away  = max(park_drive_away_data$'Max FW accel in corner per m')




