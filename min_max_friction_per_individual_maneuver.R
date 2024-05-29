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

