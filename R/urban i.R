# Leon Tyre Test Track Simulations Rural v

## Input vehicle specifications Ford Escape Kuga

#Vehicle mass in kg
m_vehicle= 1660
#Vehicle surface area in m^2
A_vehicle=2.629
#Vehicle aerodynamic drag coefficient
c_drag= 0.347
#Vehicle acceleration from 0-100 km.h^-1 in s
t_0_100kmh_vehicle= 9.2
# Fraction of vehicle mass that consists of rotating parts (kg/kg)
frac_mass_rotate_parts_vehicle = runif(n=1000, 0.13,0.15)

## Input tyre performance data

tyre_name ="Linglong"
# minimum roll coefficient (kg/t) according to EU label
c_roll_tyre_min = 9.1
# maximum roll coefficient (kg/t) according to EU label
c_roll_tyre_max = 10.5
# minimum grip index according to EU label
grip_index_tyre_min = 1.55
# maximum grip index according to EU label
grip_index_tyre_max = 1.56
# indicate track underground as "dry asphalt" or "wet asphalt"
track_underground = "dry_asphalt"


#### setup tyre grip performance simulation
grip_index_tyre = runif(n=1000,grip_index_tyre_min,grip_index_tyre_max)
#### peak friction coefficient of reference tyre under reference conditions in EU wet grip tests
mu_max_ref_tyre_wet = 0.85

# Source tyre friction funtions
source("tyre_wear_functions20230614.R")

## Setup vehicle parameters for track friction simulations

### Normal load force of vehicle (N) performed in downwards direction
normal_load_force <- f_normal_load_force(m_vehicle, grav_constant)
### Vehicle maximum acceleration constant (m.s^-2)
c_accel_max <-f_c_accel_max(t_0_100kmh_vehicle)
### mass (kg) of rotating parts
m_rotate= f_m_rotate(frac_mass_rotate_parts_vehicle, m_vehicle)


#### calculate peak friction coefficient between tyre and track
mu_max_tyre_track = f_mu_max(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet)
#### calculate linear increase constant 'x' for track
x_slip_long_force_tyre_track <- f_x_slip_long_force(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
#### brake deceleration constant of reference tyre under reference conditions in EU wet grip tests
c_full_brake_ref_tyre_wet = 0.68*grav_constant
### calculate track-tyre deceleration constant (m.s^-2) at full wheel lock braking
c_full_brake_tyre_track <- f_c_full_brake(grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
#### tyre roll coefficient as range
c_roll =runif(n=1000, c_roll_tyre_min,c_roll_tyre_max)/1000



# IDIADA track simulations

## Sector 1 'a'

sector_distance = 32 -(pi*11.7*(35/360)/2)
sector_velocity_kmh = 20
sector_start_velocity_kmh = 20
sector_end_velocity_kmh = 20
sector_underground = "dry asphalt"
sector_alpha_slope = 0
alpha_slope = sector_alpha_slope
sector_latitude = 0 
sector_corner_radius = 0
sector_bank_slope = 0
sector_corner_angle = 0
sector_decel_g = 1.4/grav_constant
c_decel= sector_decel_g*grav_constant
sector_accel_g= 1.4/grav_constant
c_accel= sector_accel_g*grav_constant

sector_1_repeats = 4

## Sector 1 straight sector simulations
IDIADA_accel_distance_sector_1 = f_accel_distance(sector_start_velocity_kmh, sector_velocity_kmh , c_accel)
IDIADA_accel_slip_sector_1 = f_accel_slip(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
IDIADA_accel_friction_work_sector_1 = f_accel_friction_work(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)

IDIADA_decel_distance_sector_1 =f_decel_distance(sector_velocity_kmh, sector_end_velocity_kmh , c_decel)
IDIADA_decel_slip_sector_1 = f_decel_brake_slip(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_decel_friction_work_sector_1 = f_decel_friction_work(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope, c_decel, m_rotate, grip_index_tyre, x_correct_road,c_full_brake_ref_tyre_wet)

IDIADA_const_speed_distance_sector_1 = f_const_speed_distance(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel)
IDIADA_const_speed_slip_sector_1 = f_const_speed_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_const_speed_friction_work_sector_1 = f_const_speed_friction_work(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_total_friction_work_sector_1 = sector_1_repeats*(IDIADA_accel_friction_work_sector_1+IDIADA_decel_friction_work_sector_1+IDIADA_const_speed_friction_work_sector_1)

## Sector 2 corner sector 
sector_start_velocity_kmh = 20
sector_end_velocity_kmh = 20
sector_velocity_kmh = 20
sector_alpha_slope = 0
sector_decel_g= 0
sector_alpha_bank_slope= 0
sector_corner_radius = 38.2/2
sector_corner_angle = 180
sector_underground = "dry asphalt"

sector_2_repeats = 2

## Sector 2 corner sector simulations
IDIADA_corner_distance_sector_2 = f_corner_distance(sector_corner_radius,sector_corner_angle)
IDIADA_corner_lat_slip_sector_2 = f_corner_lat_slip(m_vehicle , sector_velocity_kmh, sector_corner_radius,grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
IDIADA_corner_lat_friction_work_sector_2 = f_corner_lat_friction_work(sector_corner_radius,sector_corner_angle, m_vehicle , sector_velocity_kmh, grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)

IDIADA_const_speed_distance_sector_2 = f_const_speed_distance(sector_distance = IDIADA_corner_distance_sector_2,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel)
IDIADA_const_speed_slip_sector_2 = f_const_speed_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_const_speed_friction_work_sector_2 = f_const_speed_friction_work(sector_distance = IDIADA_corner_distance_sector_2,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)

IDIADA_total_friction_work_sector_2 = sector_2_repeats*(IDIADA_corner_lat_friction_work_sector_2+IDIADA_const_speed_friction_work_sector_2)

## Sector 3 corner sector 
sector_start_velocity_kmh = 20
sector_end_velocity_kmh = 20
sector_velocity_kmh = 20
sector_alpha_slope = 0
sector_decel_g= 0
sector_alpha_bank_slope= 0
sector_corner_radius = 11.7/2
sector_corner_angle = 35
sector_underground = "dry asphalt"

sector_3_repeats = 4

IDIADA_corner_distance_sector_3 = f_corner_distance(sector_corner_radius,sector_corner_angle)
IDIADA_corner_lat_slip_sector_3 = f_corner_lat_slip(m_vehicle , sector_velocity_kmh, sector_corner_radius,grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
IDIADA_corner_lat_friction_work_sector_3 = f_corner_lat_friction_work(sector_corner_radius,sector_corner_angle, m_vehicle , sector_velocity_kmh, grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)

IDIADA_const_speed_distance_sector_3 = f_const_speed_distance(sector_distance = IDIADA_corner_distance_sector_2,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel)
IDIADA_const_speed_slip_sector_3 = f_const_speed_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_const_speed_friction_work_sector_3 = f_const_speed_friction_work(sector_distance = IDIADA_corner_distance_sector_2,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)

IDIADA_total_friction_work_sector_3 = sector_3_repeats*(IDIADA_corner_lat_friction_work_sector_3+IDIADA_const_speed_friction_work_sector_3)


## straight sector b 
sector_distance = 135 -(pi*11.7*(35/360))
sector_velocity_kmh = 20
sector_start_velocity_kmh = 20
sector_end_velocity_kmh = 20
sector_underground = "dry asphalt"
sector_alpha_slope = 0
alpha_slope = sector_alpha_slope
sector_latitude = 0 
sector_corner_radius = 0
sector_bank_slope = 0
sector_corner_angle = 0
sector_decel_g = 1.4/grav_constant
c_decel= sector_decel_g*grav_constant
sector_accel_g= 1.4/grav_constant
c_accel= sector_accel_g*grav_constant

sector_4_repeats = 2

## Sector 4 straight sector simulations
IDIADA_accel_distance_sector_4 = f_accel_distance(sector_start_velocity_kmh, sector_velocity_kmh , c_accel)
IDIADA_accel_slip_sector_4 = f_accel_slip(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
IDIADA_accel_friction_work_sector_4 = f_accel_friction_work(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)

IDIADA_decel_distance_sector_4 =f_decel_distance(sector_velocity_kmh, sector_end_velocity_kmh , c_decel)
IDIADA_decel_slip_sector_4 = f_decel_brake_slip(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_decel_friction_work_sector_4 = f_decel_friction_work(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope, c_decel, m_rotate, grip_index_tyre, x_correct_road,c_full_brake_ref_tyre_wet)

IDIADA_const_speed_distance_sector_4 = f_const_speed_distance(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel)
IDIADA_const_speed_slip_sector_4 = f_const_speed_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_const_speed_friction_work_sector_4 = f_const_speed_friction_work(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
IDIADA_total_friction_work_sector_4 = sector_4_repeats*(IDIADA_accel_friction_work_sector_4+IDIADA_decel_friction_work_sector_4+IDIADA_const_speed_friction_work_sector_4)

# calculation of total friction work

IDIADA_total_friction_work =IDIADA_total_friction_work_sector_1 + IDIADA_total_friction_work_sector_2 + IDIADA_total_friction_work_sector_3 + IDIADA_total_friction_work_sector_4

IDIADA_friction_work_per_m = IDIADA_total_friction_work/500

min(IDIADA_friction_work_per_m)
max(IDIADA_friction_work_per_m)


#IDIADA_perc_accel_friction_work =  IDIADA_accel_friction_work_sector_1 / (IDIADA_total_friction_work)
#IDIADA_perc_decel_friction_work =  IDIADA_decel_friction_work_sector_1 / (IDIADA_total_friction_work)
#IDIADA_perc_const_speed_friction_work =  IDIADA_const_speed_friction_work_sector_1 / (IDIADA_total_friction_work)
#IDIADA_perc_corner_friction_work = (IDIADA_const_speed_friction_work_sector_2+IDIADA_corner_lat_friction_work_sector_2)/ (IDIADA_total_friction_work)


