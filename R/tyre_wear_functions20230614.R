# Input data

## Input vehicle specifications ##

vehicle_name = "Ford Kuga"
#Vehicle mass in kg
m_vehicle= 1660
#Vehicle surface area in m^2
A_vehicle=2.629
#Vehicle aerodynamic drag coefficient
c_drag= 0.347
#Vehicle acceleration from 0-100 km.h^-1in s
t_0_100kmh= 9.2
# Fraction of vehicle mass that consists of rotating parts (kg/kg)
frac_mass_rotate_parts_vehicle = runif(n=1000, 0.13,0.15)

## Tyre performance input data
### Roll resistance coefficient in kg/kg at dry asphalt
#c_roll= runif(n=1000,1,10)/1000
###Grip index tyre
#grip_index_tyre = runif(n=1000,1.09,1.55)
### indicate track underground as "dry asphalt" or "wet asphalt"
#track_underground = "dry_asphalt"

## Physics data ##
### Gravitational constant in m.s^-2
grav_constant=9.81

## Landscape data ##
###Density of air in kg.m^-3
rho_air=1.205
### Wind speed in m.s^-1
v_wind=runif(n=1000,-5,5)

# Vehicle performance parameter calculations
## maximum acceleration constant in m.s^-2
f_c_accel_max <-function(t_0_100kmh) {(100*(1000/3600))/t_0_100kmh}
# Total mass of rotating vehicle parts in kg
f_m_rotate <-function(frac_mass_rotate_parts_vehicle,m_vehicle) {frac_mass_rotate_parts_vehicle*m_vehicle}

## Track-tyre performance calculations
### optimal slip ratio for dry and wet asphalt conditions
if(track_underground == "dry_asphalt") {optimal_slip = runif(n=1000, 0.15,0.20)}
if(track_underground == "wet_asphalt") {optimal_slip = runif(n=1000, 0.08,0.12)}
### linearity slip and friction correction at wet and dry asphalt
if(track_underground == "wet_asphalt") {x_correct_road = 1}
if(track_underground == "dry_asphalt") {x_correct_road = runif(n=1000,1.07,1.47)}
### calculate peak friction coefficient between tyre and track
#### Peak friction coefficient function
f_mu_max_wet <- function(grip_index_tyre, mu_max_ref_tyre_wet){(grip_index_tyre*mu_max_ref_tyre_wet)/1.25}
f_mu_max <- function(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet){f_mu_max_wet(grip_index_tyre, mu_max_ref_tyre_wet)*x_correct_road}

### Peak friction coefficient of reference tyre in wet grip testing 
mu_max_ref_tyre_wet = 0.85
### brake deceleration constant of reference tyre in reference wet grip testing in m.s^-2
c_full_brake_ref_tyre_wet = 0.68*grav_constant
### brake deceleration of candidate tyre in wet grip testing in m.s^-2
f_c_full_brake_wet <- function (grip_index_tyre, c_full_brake_ref_tyre_wet){(grip_index_tyre*c_full_brake_ref_tyre_wet)/1.25}
### brake deceleration of candidate tyre correction from wet grip testing to specific road track m.s^-2
f_c_full_brake <- function (grip_index_tyre, c_full_brake_ref_tyre_wet, x_correct_road){f_c_full_brake_wet(grip_index_tyre, c_full_brake_ref_tyre_wet)*x_correct_road}
### linearity at low slip regime between friction coefficient and slip
f_x_slip_long_force <- function(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip){f_mu_max(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet)/optimal_slip}
x_slip_long_force = f_x_slip_long_force(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)

### Normal load force function
f_normal_load_force <- function(m_vehicle, grav_constant){m_vehicle*grav_constant}
### Normal load force in N
normal_load_force <- f_normal_load_force(m_vehicle, grav_constant)
### Linearity at low slip regime

# Friction functions per sector

## sector acceleration
f_accel_time<-function(sector_start_velocity_kmh, sector_velocity_kmh,c_accel){-(sector_start_velocity_kmh*1000/3600-sector_velocity_kmh*1000/3600)/c_accel}
f_accel_distance <- function(sector_start_velocity_kmh, sector_velocity_kmh , c_accel ) {sector_start_velocity_kmh*1000/3600*f_accel_time(sector_start_velocity_kmh, sector_velocity_kmh, c_accel)+1/2*c_accel*(f_accel_time(sector_start_velocity_kmh, sector_velocity_kmh, c_accel))^2}
f_accel_v_start <- function(sector_velocity_kmh,sector_start_velocity_kmh){pmin(sector_velocity_kmh,sector_start_velocity_kmh)*1000/3600}
f_accel_average_v <- function(sector_start_velocity_kmh, sector_velocity_kmh){(f_accel_v_start(sector_velocity_kmh, sector_start_velocity_kmh) +sector_velocity_kmh*1000/3600)/2}
f_accel_drag_force <- function (c_drag, A_vehicle, rho_air, sector_start_velocity_kmh, sector_velocity_kmh, v_wind){pmax(0,c_drag*A_vehicle*rho_air*(((f_accel_average_v(sector_start_velocity_kmh,sector_velocity_kmh))+v_wind)^2)/2)}
f_accel_roll_force <- function(c_roll, m_vehicle, grav_constant){c_roll*m_vehicle*grav_constant}
f_accel_uphill_slope_force <- function(m_vehicle, grav_constant, alpha_slope){pmax(0,m_vehicle*grav_constant*sin(alpha_slope))}
f_accel_inert_force<- function(m_vehicle, m_rotate, c_accel) {(m_vehicle+m_rotate)*c_accel}
f_accel_long_force<-function (c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)
{f_accel_drag_force(c_drag, A_vehicle, rho_air, sector_start_velocity_kmh, sector_velocity_kmh, v_wind)+f_accel_roll_force(c_roll, m_vehicle, grav_constant)
  +f_accel_uphill_slope_force(m_vehicle, grav_constant, alpha_slope)+f_accel_inert_force(m_vehicle, m_rotate, c_accel)}

f_accel_mu <- function(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)
  {f_accel_long_force(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)/f_normal_load_force(m_vehicle, grav_constant)}

f_accel_slip <- function (c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
{pmin(optimal_slip,(f_accel_mu(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)/
                      f_x_slip_long_force(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)))}

f_accel_friction_work <- function(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)
{f_accel_distance(sector_start_velocity_kmh, sector_velocity_kmh , c_accel)*f_accel_long_force(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)*f_accel_slip(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)}


## sector deceleration
f_decel_time<-function(sector_velocity_kmh, sector_end_velocity_kmh,c_decel){((sector_velocity_kmh*1000/3600)-(sector_end_velocity_kmh*1000/3600))/c_decel}
f_decel_distance <- function(sector_velocity_kmh, sector_end_velocity_kmh , c_decel ) {((sector_velocity_kmh*1000/3600)^2-(sector_end_velocity_kmh*1000/3600)^2)/(2*c_decel)}
f_decel_v_start <- function(sector_velocity_kmh,sector_end_velocity_kmh){pmax(sector_velocity_kmh,sector_end_velocity_kmh)*1000/3600}  
f_decel_average_v <-function (sector_velocity_kmh, sector_end_velocity_kmh){(f_decel_v_start(sector_velocity_kmh,sector_end_velocity_kmh)+sector_end_velocity_kmh*1000/3600)/2}
f_decel_drag_force <- function (c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind){pmax(0,c_drag*A_vehicle*rho_air*(((f_decel_average_v(sector_velocity_kmh,sector_end_velocity_kmh))+v_wind)^2)/2)}
f_decel_roll_force <- function(c_roll, m_vehicle, grav_constant){c_roll*m_vehicle*grav_constant}
f_decel_uphill_slope_force <- function(m_vehicle, grav_constant, alpha_slope){pmax(0,m_vehicle*grav_constant*sin(alpha_slope))}
f_decel_resist_force <- function (c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope)
  {f_decel_drag_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind)+f_decel_roll_force(c_roll, m_vehicle, grav_constant)+f_decel_uphill_slope_force(m_vehicle, grav_constant, alpha_slope)}
f_c_decel_resist <-function (c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate)
  {f_decel_resist_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope)/(m_vehicle+m_rotate)}

f_downhill_slope_force <- function (m_vehicle, grav_constant, alpha_slope){pmin(0,m_vehicle*grav_constant*sin(alpha_slope))} 
f_c_accel_downhill_slope_force <-function(m_vehicle, grav_constant, alpha_slope,m_rotate){-(f_downhill_slope_force(m_vehicle, grav_constant, alpha_slope)/(m_vehicle+m_rotate))}
f_c_add_brake <-function(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate)
{pmax(0,c_decel - f_c_decel_resist(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate) 
        -f_c_accel_downhill_slope_force(m_vehicle, grav_constant, alpha_slope,m_rotate))}
f_decel_add_brake_force <- function(m_vehicle, m_rotate, c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, grav_constant ,alpha_slope)
  {(m_vehicle+m_rotate)*(f_c_add_brake(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate))}

f_decel_long_force <- function(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope)
{f_decel_resist_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope)
  +f_decel_add_brake_force(m_vehicle, m_rotate, c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, grav_constant ,alpha_slope)}

f_decel_brake_slip <- function (c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
{pmin(1,f_c_add_brake(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate)/
        f_c_full_brake(grip_index_tyre, c_full_brake_ref_tyre_wet, x_correct_road))}

f_decel_mu <- function(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope)
{f_decel_long_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope)/f_normal_load_force(m_vehicle, grav_constant)}

f_decel_wheelspin_slip <- function (c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope,grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
{pmin(optimal_slip,(f_decel_mu(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope)/
                      f_x_slip_long_force(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)))}

f_decel_slip <-function(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet, mu_max_ref_tyre_wet, optimal_slip)
{f_decel_brake_slip(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)+
    f_decel_wheelspin_slip(c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope,grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)}
f_decel_friction_work<- function(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope, c_decel, m_rotate, grip_index_tyre, x_correct_road,c_full_brake_ref_tyre_wet)
{f_decel_distance(sector_velocity_kmh, sector_end_velocity_kmh ,c_decel)*f_decel_long_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope)*
    f_decel_slip(c_decel, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant ,alpha_slope, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet, mu_max_ref_tyre_wet, optimal_slip)}


# sector constant speed

f_const_speed_distance <- function (sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel )
  {sector_distance-f_accel_distance(sector_start_velocity_kmh, sector_velocity_kmh , c_accel)-f_decel_distance(sector_velocity_kmh, sector_end_velocity_kmh , c_decel)}

f_const_speed_drag_force <-function (c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind)
  {pmax(0,c_drag*A_vehicle*rho_air*(((sector_velocity_kmh*1000/3600)+v_wind)^2)/2)}
f_const_speed_roll_force <- function(c_roll, m_vehicle, grav_constant) {c_roll*m_vehicle*grav_constant}
f_const_speed_uphill_slope_force <- function(m_vehicle, grav_constant, alpha_slope) {pmax(0,m_vehicle*grav_constant*sin(alpha_slope))}

f_const_speed_downhill_slope_force <- function (m_vehicle, grav_constant, alpha_slope){pmin(0,m_vehicle*grav_constant*sin(alpha_slope))} 
f_const_speed_add_brake_force <- function(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll)
{pmax(0,-f_const_speed_downhill_slope_force(m_vehicle, grav_constant, alpha_slope)-f_const_speed_drag_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind)
          -f_const_speed_roll_force(c_roll, m_vehicle, grav_constant))}

f_const_speed_resist_force <- function(c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope)
{f_const_speed_drag_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind)+f_const_speed_roll_force(c_roll, m_vehicle, grav_constant)+f_const_speed_uphill_slope_force(m_vehicle, grav_constant, alpha_slope)}

f_const_speed_long_force <- function(c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope)
  {f_const_speed_drag_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind) +  f_const_speed_roll_force(c_roll, m_vehicle, grav_constant) + f_const_speed_uphill_slope_force(m_vehicle, grav_constant, alpha_slope)+f_const_speed_add_brake_force(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll)}

f_const_speed_wheelspin_slip <- function(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope)
{pmin(optimal_slip, (f_const_speed_resist_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope)/f_normal_load_force(m_vehicle,grav_constant)/x_slip_long_force))}

f_const_speed_c_add_brake <- function(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate)
{f_const_speed_add_brake_force(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll)/(m_vehicle+m_rotate)}

f_const_speed_brake_slip <-function(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)
{pmin(1,f_const_speed_c_add_brake(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate)/f_c_full_brake(grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet))}

f_const_speed_slip<-function(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre)
{f_const_speed_wheelspin_slip(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope)+f_const_speed_brake_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)}
f_const_speed_friction_work <-function(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet )
{f_const_speed_distance(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel)*f_const_speed_long_force(c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope)*f_const_speed_slip(m_vehicle, grav_constant, alpha_slope, c_drag, A_vehicle, rho_air, sector_velocity_kmh, v_wind,c_roll, m_rotate, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre_wet)}

# sector total friction work
f_sector_longitude_friction_work <- function(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre)
{f_accel_friction_work(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air,sector_start_velocity_kmh, sector_velocity_kmh, v_wind, c_roll, m_vehicle, grav_constant, alpha_slope, m_rotate, c_accel)
  +f_decel_friction_work(optimal_slip, x_slip_long_force, c_drag, A_vehicle, rho_air, sector_velocity_kmh, sector_end_velocity_kmh , v_wind, c_roll, m_vehicle, grav_constant , alpha_slope, c_decel, m_rotate, grip_index_tyre, x_correct_road,c_full_brake_ref_tyre)
  +f_const_speed_friction_work(sector_distance,sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre)}
  
# sector lateral friction work
f_corner_distance <- function(sector_corner_radius,sector_corner_angle){(sector_corner_angle/(360)*2*sector_corner_radius*pi)}
f_corner_centripet_force <- function(m_vehicle , sector_velocity_kmh, sector_corner_radius) {m_vehicle*((sector_velocity_kmh*1000/3600)^2)/sector_corner_radius}
f_corner_bank_force <- function(grav_constant, sector_alpha_bank_slope, m_vehicle){grav_constant*sin(sector_alpha_bank_slope)*m_vehicle}
f_corner_lat_force <- function(m_vehicle , sector_velocity_kmh, sector_corner_radius,grav_constant, sector_alpha_bank_slope )
                               {f_corner_centripet_force(m_vehicle , sector_velocity_kmh, sector_corner_radius)+f_corner_bank_force(grav_constant, sector_alpha_bank_slope, m_vehicle)}

f_x_slip_lat_force <-function(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip){1/7*f_x_slip_long_force(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)}

f_corner_lat_slip <- function(m_vehicle , sector_velocity_kmh, sector_corner_radius,grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)
{f_corner_lat_force(m_vehicle , sector_velocity_kmh, sector_corner_radius,grav_constant, sector_alpha_bank_slope)/f_normal_load_force(m_vehicle,grav_constant)/f_x_slip_lat_force(grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)}

 
f_corner_lat_friction_work <- function(sector_corner_radius,sector_corner_angle, m_vehicle , sector_velocity_kmh, grav_constant, alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip )
{f_corner_distance(sector_corner_radius,sector_corner_angle)*f_corner_lat_force(m_vehicle , sector_velocity_kmh, sector_corner_radius , grav_constant, alpha_bank_slope)*f_corner_lat_slip(m_vehicle , sector_velocity_kmh, sector_corner_radius,grav_constant, sector_alpha_bank_slope, grip_index_tyre, x_correct_road, mu_max_ref_tyre_wet, optimal_slip)}

f_corner_long_friction_work <- function (sector_corner_radius, sector_corner_angle, sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre)  
{f_sector_longitude_friction_work(f_corner_distance(sector_corner_radius,sector_corner_angle), sector_start_velocity_kmh, sector_velocity_kmh , c_accel, sector_end_velocity_kmh , c_decel, c_drag, A_vehicle, rho_air, v_wind , c_roll, m_vehicle, grav_constant , alpha_slope, grip_index_tyre, x_correct_road, c_full_brake_ref_tyre )}

# Landscape slope in %
#grade_slope = runif(n=1000,-10,10)/100
#alpha_slope = atan(grade_slope)*(180/pi)
#grade_bank_slope = runif(n=1000,-10,10)/100
#alpha_bank_slope = atan(grade_slope)*(180/pi)