# Leon Tyre Test Track Simulations

# Source base functions for friction simulations
source("R/Base functions.R")
# Source setup of vehicle and tyre data
source("R/IDIADA_vehicle_and_tyre_data.R")

# IDIADA track simulations

## Sector 1 straight sector input
sector_distance = 3193-940
sector_velocity_kmh = 130
sector_start_velocity_kmh = 80
sector_end_velocity_kmh = 80
sector_underground = "dry asphalt"
sector_alpha_slope = 0
alpha_slope = 0
sector_latitude = 0 
sector_corner_radius = 1/0
sector_bank_slope = atan(0.01)
sector_corner_angle = 0
sector_decel_g = 0.3
c_decel= 0.3*grav_constant
sector_accel_g= 0.17
c_accel= sector_accel_g*grav_constant

## Sector 1 straight sector simulations
### Acceleration sector 1
accel_distance_sector_1 = f_accel_distance(v_start=sector_start_velocity_kmh/3.6, v_end= sector_velocity_kmh/3.6 , c_accel)
accel_long_force_sector_1 = f_accel_long_force(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start = sector_start_velocity_kmh/3.6, v_end = sector_velocity_kmh/3.6, v_wind, alpha_slope, m_rotate, c_accel)
accel_long_slip_sector_1 = f_accel_long_slip(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel =sector_start_velocity_kmh/3.6, v_end_accel =sector_end_velocity_kmh/3.6, v_wind, alpha_slope, m_rotate, c_accel, mu_max_tyre_track, optimal_slip_ratio_track)
accel_long_friction_work_sector_1 = accel_distance_sector_1 * accel_long_force_sector_1 *accel_long_slip_sector_1
accel_lat_force_sector_1 = f_lat_force(m_vehicle , v_vehicle =mean(sector_start_velocity_kmh/3.6, sector_velocity_kmh/3.6), r_corner = sector_corner_radius, grav_constant, alpha_bank_slope = sector_bank_slope)
accel_lat_slip_sector_1 = f_lat_slip(m_vehicle , v_vehicle=mean(sector_start_velocity_kmh/3.6, sector_velocity_kmh/3.6) , r_corner = sector_corner_radius, grav_constant, alpha_bank_slope = sector_bank_slope, mu_max_tyre_track, optimal_slip_ratio_track)
accel_lat_friction_work_sector_1 = accel_distance_sector_1 * accel_lat_force_sector_1 * accel_lat_slip_sector_1

decel_distance_sector_1 = f_decel_distance(v_start = sector_velocity_kmh/3.6, v_end = sector_end_velocity_kmh/3.6, c_decel)
decel_long_force_sector_1 = f_decel_long_force(m_vehicle, c_roll, grav_constant, rho_air, v_start_decel = sector_velocity_kmh/3.6, v_end_decel = sector_end_velocity_kmh/3.6, v_wind, alpha_slope, c_decel)
decel_long_slip_sector_1 = f_decel_long_slip(c_decel, m_vehicle, m_rotate, c_roll, grav_constant, rho_air, v_start_decel=sector_velocity_kmh/3.6, v_end_decel=sector_end_velocity_kmh/3.6, v_wind, alpha_slope,mu_max_tyre_track, optimal_slip_ratio_track, c_max_brake)



IDIADA_decel_distance_sector_1 =f_decel_distance(sector_velocity_kmh, sector_end_velocity_kmh , c_decel)
IDIADA_decel_long_force_sector_1 = f_decel_long_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope)
IDIADA_decel_slip_sector_1 = f_decel_brake_slip(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_decel_friction_work_sector_1 = f_decel_friction_work(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope, c_decel, m_rotate, grip_index_tyre, x_correct_road,c_full_brake_ref_tyre_wet)

IDIADA_const_speed_distance_sector_1 = f_const_speed_distance(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel)
IDIADA_const_speed_long_force_sector_1 = f_const_speed_long_force(c_drag,
                                                                  A_vehicle,
                                                                  rho_air,
                                                                  sector_velocity_kmh,
                                                                  v_wind ,
                                                                  c_roll,
                                                                  m_vehicle,
                                                                  grav_constant ,
                                                                  alpha_slope)
IDIADA_const_speed_slip_sector_1 = f_const_speed_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_const_speed_friction_work_sector_1 = f_const_speed_friction_work(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)

## Sector 2 corner sector input
sector_start_velocity_kmh = 80
sector_end_velocity_kmh = 80
sector_velocity_kmh = 80
sector_alpha_slope = 0
sector_decel_g= 0.3
sector_alpha_bank_slope= atan(0.01)*180/pi
sector_corner_radius = 940/2
sector_corner_angle = 180
sector_underground = "dry asphalt"

## Sector 2 corner sector simulations
IDIADA_corner_distance_sector_2 = f_corner_distance(sector_corner_radius,sector_corner_angle)
IDIADA_corner_lat_force_sector_2 = f_corner_lat_force(m_vehicle, sector_velocity_kmh, sector_corner_radius, grav_constant, sector_alpha_bank_slope)
IDIADA_corner_lat_slip_sector_2 = f_corner_lat_slip(m_vehicle , sector_velocity_kmh, sector_corner_radius,grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
IDIADA_corner_lat_friction_work_sector_2 = f_corner_lat_friction_work(sector_corner_radius,sector_corner_angle, m_vehicle , sector_velocity_kmh, grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)

IDIADA_const_speed_distance_sector_2 = f_const_speed_distance(sector_distance = IDIADA_corner_distance_sector_2,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel)
IDIADA_corner_long_force_sector_2 = f_const_speed_long_force(c_drag,
                                                                  A_vehicle,
                                                                  rho_air,
                                                                  sector_velocity_kmh,
                                                                  v_wind ,
                                                                  c_roll,
                                                                  m_vehicle,
                                                                  grav_constant ,
                                                                  alpha_slope)
IDIADA_const_speed_slip_sector_2 = f_const_speed_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_const_speed_friction_work_sector_2 = f_const_speed_friction_work(sector_distance = IDIADA_corner_distance_sector_2,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)

IDIADA_total_friction_work = IDIADA_accel_friction_work_sector_1 + IDIADA_decel_friction_work_sector_1+IDIADA_const_speed_friction_work_sector_1+IDIADA_corner_lat_friction_work_sector_2+IDIADA_const_speed_friction_work_sector_2


maneuver_simulation_data <- data.frame (maneuver = c (1:8), 
                                        distance = c(IDIADA_accel_distance_sector_1, IDIADA_const_speed_distance_sector_1, IDIADA_decel_distance_sector_1, IDIADA_corner_distance_sector_2, IDIADA_accel_distance_sector_1, IDIADA_const_speed_distance_sector_1, IDIADA_decel_distance_sector_1, IDIADA_corner_distance_sector_2),
                                        "min. long. force" = c (min(IDIADA_accel_long_force_sector_1), min(IDIADA_const_speed_long_force_sector_1), min (IDIADA_decel_long_force_sector_1), min(IDIADA_corner_long_force_sector_2), min(IDIADA_accel_long_force_sector_1), min(IDIADA_const_speed_long_force_sector_1), min (IDIADA_decel_long_force_sector_1), min(IDIADA_corner_long_force_sector_2)),
                                        "median long. force" = c (median(IDIADA_accel_long_force_sector_1), median(IDIADA_const_speed_long_force_sector_1), median (IDIADA_decel_long_force_sector_1), median(IDIADA_corner_long_force_sector_2), median(IDIADA_accel_long_force_sector_1), median(IDIADA_const_speed_long_force_sector_1), median (IDIADA_decel_long_force_sector_1), median(IDIADA_corner_long_force_sector_2)),
                                        "max. long. force" = c (max(IDIADA_accel_long_force_sector_1), max(IDIADA_const_speed_long_force_sector_1), max (IDIADA_decel_long_force_sector_1), max(IDIADA_corner_long_force_sector_2), max(IDIADA_accel_long_force_sector_1), max(IDIADA_const_speed_long_force_sector_1), max (IDIADA_decel_long_force_sector_1), max(IDIADA_corner_long_force_sector_2)),
                                        "min. long. slip" = c (min(IDIADA_accel_slip_sector_1), min(IDIADA_const_speed_slip_sector_1), min (IDIADA_decel_slip_sector_1), min(IDIADA_const_speed_slip_sector_2), min(IDIADA_accel_slip_sector_1), min(IDIADA_const_speed_slip_sector_1), min (IDIADA_decel_slip_sector_1), min(IDIADA_const_speed_slip_sector_2)),
                                        "median. long. slip" = c (median(IDIADA_accel_slip_sector_1), median(IDIADA_const_speed_slip_sector_1), median (IDIADA_decel_slip_sector_1), median(IDIADA_const_speed_slip_sector_2), median(IDIADA_accel_slip_sector_1), median(IDIADA_const_speed_slip_sector_1), median(IDIADA_decel_slip_sector_1), median(IDIADA_const_speed_slip_sector_2)),
                                        "max. long. slip" = c (max(IDIADA_accel_slip_sector_1), max(IDIADA_const_speed_slip_sector_1), max(IDIADA_decel_slip_sector_1), max(IDIADA_const_speed_slip_sector_2), max(IDIADA_accel_slip_sector_1), max(IDIADA_const_speed_slip_sector_1), max(IDIADA_decel_slip_sector_1), max(IDIADA_const_speed_slip_sector_2)),
                                        "min. long. friction" = c (min(IDIADA_accel_friction_work_sector_1), min(IDIADA_const_speed_friction_work_sector_1), min (IDIADA_decel_friction_work_sector_1), min(IDIADA_const_speed_friction_work_sector_2), min(IDIADA_accel_friction_work_sector_1), min(IDIADA_const_speed_friction_work_sector_1), min (IDIADA_decel_friction_work_sector_1), min(IDIADA_const_speed_friction_work_sector_2)),
                                        "median long. friction" = c (median(IDIADA_accel_friction_work_sector_1), median(IDIADA_const_speed_friction_work_sector_1), median (IDIADA_decel_friction_work_sector_1), median(IDIADA_const_speed_friction_work_sector_2), median(IDIADA_accel_friction_work_sector_1), median(IDIADA_const_speed_friction_work_sector_1), median (IDIADA_decel_friction_work_sector_1), median(IDIADA_const_speed_friction_work_sector_2)),
                                        "max. long. friction" = c (max(IDIADA_accel_friction_work_sector_1), max(IDIADA_const_speed_friction_work_sector_1), max (IDIADA_decel_friction_work_sector_1), max(IDIADA_const_speed_friction_work_sector_2), max(IDIADA_accel_friction_work_sector_1), max(IDIADA_const_speed_friction_work_sector_1), max (IDIADA_decel_friction_work_sector_1), max(IDIADA_const_speed_friction_work_sector_2)),
                                        "lat. force" = c(0,0,0, IDIADA_corner_lat_force_sector_2,0,0,0,IDIADA_corner_lat_force_sector_2),
                                        "min. lat. slip" = c(0,0,0, min(IDIADA_corner_lat_slip_sector_2),0,0,0,min(IDIADA_corner_lat_slip_sector_2)),
                                        "median lat. slip" = c(0,0,0, median(IDIADA_corner_lat_slip_sector_2),0,0,0,median(IDIADA_corner_lat_slip_sector_2)),
                                        "max. lat. slip" = c(0,0,0, max(IDIADA_corner_lat_slip_sector_2),0,0,0,max(IDIADA_corner_lat_slip_sector_2)),
                                        "min. lat. friction" = c(0,0,0, min(IDIADA_corner_lat_friction_work_sector_2),0,0,0,min(IDIADA_corner_lat_friction_work_sector_2)),
                                        "median lat. friction" = c(0,0,0, median(IDIADA_corner_lat_friction_work_sector_2),0,0,0,median(IDIADA_corner_lat_friction_work_sector_2)),
                                        "max. lat. friction" = c(0,0,0, max(IDIADA_corner_lat_friction_work_sector_2),0,0,0,max(IDIADA_corner_lat_friction_work_sector_2)))




IDIADA_perc_accel_friction_work =  IDIADA_accel_friction_work_sector_1 / (IDIADA_total_friction_work)
IDIADA_perc_decel_friction_work =  IDIADA_decel_friction_work_sector_1 / (IDIADA_total_friction_work)
IDIADA_perc_const_speed_friction_work =  IDIADA_const_speed_friction_work_sector_1 / (IDIADA_total_friction_work)
IDIADA_perc_corner_friction_work = (IDIADA_const_speed_friction_work_sector_2+IDIADA_corner_lat_friction_work_sector_2)/ (IDIADA_total_friction_work)

IDIADA_friction_work_per_km = IDIADA_total_friction_work/(sector_distance+IDIADA_const_speed_distance_sector_2)



## IDIADA summary data
IDIADA_simulation_data <- data.frame (sector = c("acceleration", "deceleration", "constant speed", "corner"),
                           "Distance (m)" = c(IDIADA_accel_distance_sector_1, IDIADA_decel_distance_sector_1, IDIADA_const_speed_distance_sector_1 , IDIADA_corner_distance_sector_2),
                           "Slip (min)" = c(min(IDIADA_accel_slip_sector_1), min(IDIADA_decel_slip_sector_1), min(IDIADA_const_speed_slip_sector_1), min(IDIADA_const_speed_slip_sector_2)),
                           "Slip (max)" = c(max(IDIADA_accel_slip_sector_1), max(IDIADA_decel_slip_sector_1), max(IDIADA_const_speed_slip_sector_1), max(IDIADA_const_speed_slip_sector_2)),
                           "Lateral slip (min)"=c(0,0,0,min(IDIADA_corner_lat_slip_sector_2)),
                           "Lateral slip (max)"=c(0,0,0,max(IDIADA_corner_lat_slip_sector_2)),
                           "Friction Work (J) (min)" = c(min(IDIADA_accel_friction_work_sector_1), min(IDIADA_decel_friction_work_sector_1), min(IDIADA_const_speed_friction_work_sector_1), min(IDIADA_const_speed_friction_work_sector_2)),
                           "Friction Work (J) (max)" = c(max(IDIADA_accel_friction_work_sector_1), max(IDIADA_decel_friction_work_sector_1), max(IDIADA_const_speed_friction_work_sector_1), max(IDIADA_const_speed_friction_work_sector_2)),
                           "Lateral friction work (J) (min)" = c(0,0,0, min(IDIADA_corner_lat_friction_work_sector_2)),
                           "Lateral friction work (J) (min)" = c(0,0,0, max(IDIADA_corner_lat_friction_work_sector_2)),
                           "percentage of total friction (min)" = c(min(IDIADA_perc_accel_friction_work),min(IDIADA_perc_decel_friction_work), min(IDIADA_perc_const_speed_friction_work), min(IDIADA_perc_corner_friction_work)),
                           "percentage of total friction (max)" = c(max(IDIADA_perc_accel_friction_work),max(IDIADA_perc_decel_friction_work), max(IDIADA_perc_const_speed_friction_work), max(IDIADA_perc_corner_friction_work)))

# OLD!

## Setup track-tyre interaction parameters for track friction simulations under wet and dry conditions
### optimal slip ratio for dry and wet asphalt conditions
#if(track_underground == "dry_asphalt") {optimal_slip = runif(n=1000, 0.15,0.20)}
#if(track_underground == "wet_asphalt") {optimal_slip_= runif(n=1000, 0.08,0.12)}
