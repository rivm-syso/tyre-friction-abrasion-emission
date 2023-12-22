### List of parameters

## Vehicle parameters

#'@param A_vehicle Frontal area of vehicle in (m^2)
#'@param c_drag Aerodynamic drag coefficient of the vehicle (unitless)
#'@param m_rotate Mass of the rotating parts (kg)
#'@param m_vehicle Mass of the vehicle (kg)

## Tyre quality and design parameters
#'@param c_roll The roll coefficient of the tyre (kg/kg)
#'@param grip_index_tyre Tyre grip index number from EU label
#'@param c_max_brake The maximum braking constant the tyres can achieve (m/s^2)
#'@param wet_mu_max_ref_tyre The friction coefficient of an EU reference tyre on wet asphalt (0.85)

## Road surface underground parameters

#'@param apha_bank_slope Bank slope of the road in latitudinal direction (degrees)
#'@param alpha_slope Slope of the road in longitudinal direction (degrees)' 
#'@param optimal_ratio_slip_track Optimal slip ratio of the track underground
#'@param r_corner Radius of the corner (m)
#'@param x_correct_mu_max_track A correction factor for the maximum friction coefficient across different undergrounds
#'
## Landscape parameters
#'@param rho_air Density of air (km/m^3)
#'@param v_wind Wind velocity (m/s)
#
## General physics parameters
#'@param grav_constant Gravitational constant (m/s^2)
#
## Driving maneuver parameters
#
#'@param c_accel Acceleration constant of an acceleration maneuver (m/s^2) 
#'@param c_decel Deceleration constant of a deceleration maneuver (m/s^2)
#'@param v_end_accel Vehicle velocity at end of the acceleration event in m/s
#'@param v_start_accel Vehicle velocity at start of an acceleration maneuver in m/s
#'@param v_end_decel Vehicle velocity at end of the deceleration event in m/s
#'@param v_start_decel Vehicle velocity at start of a deceleration maneuver in m/s
#'@param v_vehicle Velocity of driving maneuver (m/s)
#
## Parameters for calibration (now set as 7 according to literature)
#'@param x_linear_lat_vs_long_slip A factor to translate increase of slip by lateral mu vs increase of slip by longitidunal mu 
#
#### Friction Force Functions
#' Friction force
#' These functions are used to calculate the friction force. The functions include
#' f_roll_force, f_drag_force, f_slope_force, f_brake_force, f_accel_force, f_c_decel_inert, 
#' f_centripet_force and f_bank_force. 
#'
#'@section Roll resistance force in N:
#'
f_roll_force <- function(c_roll, m_vehicle, grav_constant) {c_roll*m_vehicle*grav_constant}

#'@section Drag resistance force in N:

f_drag_force <- function(c_drag, A_vehicle, rho_air, v_vehicle, v_wind) {pmax(0,c_drag*A_vehicle*rho_air*(((v_vehicle)+v_wind)^2)/2)}

#'@section Slope force in N:

f_slope_force <- function(m_vehicle, grav_constant, alpha_slope) {m_vehicle*grav_constant*sin(alpha_slope)}

#'@section Inertia force upon acceleration in N: 

f_accel_inert_force <- function(m_vehicle, m_rotate, c_accel) {(m_vehicle+m_rotate)*c_accel}

#'@section Total longitudinal force during acceleration
#'The total resistant forces during acceleration are the inertia force plus roll, drag and uphill slope force. 
#'In case slope is negative, slope force becomes propulsive instead of resistive and is therefor limited to 0.

f_accel_force <- function (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel)
{f_roll_force(c_roll, m_vehicle, grav_constant) +
    f_drag_force(c_drag, A_vehicle, rho_air, v_vehicle=mean(v_start_accel, v_end_accel), v_wind) +
    pmax(0,f_slope_force(m_vehicle, grav_constant, alpha_slope)) +
    f_accel_inert_force(m_vehicle, m_rotate, c_accel)}

#'@section Inertia force upon deceleration
#'
#The total inertia forces acting on the vehicle the moment a deceleration maneuvers commences (the driver lays foot off gas)
#is equal to the sum of all resistant forces on the vehicle at that moment which are roll force, drag force and uphill slope force. 
#' In case of downhill slope, the slope force is negative to the resistant force, 
#' so that more brake force is needed to achieve the desired deceleration constant

f_decel_resist_force <- function (m_vehicle, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope)
{f_roll_force(c_roll, m_vehicle, grav_constant)
    + f_drag_force(c_drag, A_vehicle, rho_air, v_vehicle = mean(v_start_decel,v_end_decel), v_wind)
  + f_slope_force(m_vehicle, grav_constant, alpha_slope)}

#'@section Maximum brake force
# Brake force acting on the vehicle when the maximum brake constant of the tyres is achieved

f_max_brake_force <- function(m_vehicle, m_rotate, c_max_brake) {(m_vehicle+m_rotate)*c_max_brake}

#'@section Brake force needed upon deceleration

# The additional brake force needed upon deceleration is calculated from the deceleration constant of the maneuver and the other resistant forces already slowing the vehicle down

f_decel_brake_force <- function(c_decel, m_vehicle, m_rotate, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope)
{pmax(0,(c_decel * (m_vehicle + m_rotate)) 
      - f_decel_resist_force(m_vehicle, m_rotate, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope))}

#'@section Brake force needed to remain under speed limit at downhill slope

# The driver may need to brake to remain under speed limit at roads with steep downhill slopes and low speed limits. 
# First the propulsive force in N caused by the downhill slope  of the road is simulated as a force negative to the resistant forces

f_downhill_slope_force <- function(m_vehicle, grav_constant, alpha_slope)
  { pmax(0, m_vehicle * grav_constant * sin(alpha_slope))}

# Then the resistant forces already slowing the vehicle down are simulated, which are roll resistant force and drag force

f_downhill_resist_force <- function (c_roll, m_vehicle, grav_constant, c_drag, rho_air, A_vehicle, v_vehicle, v_wind)
{f_roll_force(c_roll, m_vehicle, grav_constant)
  + f_drag_force(c_drag, A_vehicle, rho_air, v_vehicle, v_wind)}

# Additional brake force is needed in case the downhill slope force is greater than the resistant force

f_downhill_brake_force <- function (m_vehicle, grav_constant, alpha_slope, c_roll, c_drag, A_vehicle, rho_air, v_vehicle, v_wind)
{-(pmax(0,
      f_downhill_slope_force(m_vehicle, grav_constant, alpha_slope)
      + f_downhill_resist_force(c_roll, grav_constant, c_drag, rho_air, A_vehicle, v_vehicle, v_wind)))}

#'@section Centripetal force in cornering in N:

f_centripet_force <- function(m_vehicle , v_vehicle , r_corner) {m_vehicle*(v_vehicle^2)/r_corner}

#'@section Bank force in cornering in N:
#'Bank force slope is positive if the high side of the bank is on the outside of the corner. 


f_bank_force <-function(grav_constant, alpha_bank_slope, m_vehicle){grav_constant*sin(alpha_bank_slope)*m_vehicle}

#'@section Resultant latitudinal force
#'Then bank slope force works in opposite direction to the centripetal force, 
#'so that the resultant lateral forces on the tyres is the centripetal force minus the bank force.
#'In case the bank slope is negative,centripetal and bank force work in the same direction. 
#'In case the bank slope force is greater than the centripetal force, 
#'the resultant latitudinal force equals the bank force minus the centripetal force.
#'The resultant latitudinal force is not to be negative. 
#'Therefore, the rules above are included in the routine as ((centripet_force - bank_force)^2)^0.5

f_latitude_force <- function(m_vehicle , v_vehicle , r_corner, grav_constant, alpha_bank_slope)
  {((f_centripet_force(m_vehicle , v_vehicle , r_corner) - f_bank_force(grav_constant, alpha_bank_slope, m_vehicle) )^2 )^0.5} 

#'@section Normal load force in N:
#' Normal load force perpendicular to longitudinal directon
#'
f_normal_load_force <- function(alpha_slope,m_vehicle, grav_constant){cos(alpha_slope)*(m_vehicle*grav_constant)}

# Normal force perpendicular to latitudinal direction

f_lat_normal_load_force <- function(alpha_bank_slope,m_vehicle, grav_constant)
{cos(alpha_bank_slope)*(m_vehicle*grav_constant)}


#### Distance and time functions ####
#' Time and distance functions
#' 
#' These functions are used to calculate the distances and times over which driving maneuvers 
#' take place. The functions include f_accel_time, f_accel_distance, f_decel_time and 
#' f_decel_distance and f_corner_distance. 
#' 
#' The acceleration time is calculated from the starting velocity (m/s), end velocity (m/s) and accelaration constant (m/s^2) of the maneuver
#'@section Acceleration time in s: 
#'
f_accel_time<-function(v_start,v_end,c_accel){-(v_start-v_end)/c_accel}

#'@section Acceleration distance in m:
#' The acceleration distance is calculated from the starting velocity (m/s), end velocity (m/s) and accelaration constant (m/s^2) of the maneuver

f_accel_distance <- function(v_start , v_end , c_accel ) 
{v_start*f_acceltime(v_start,v_end,c_accel)
  +1/2*c_accel*f_accel_time(v_start,v_end,c_accel)^2}

#'@section Deceleration time in s:
#'The deceleration time is calculated from the starting velocity (m/s), end velocity (m/s) and decelaration constant (m/s^2) of the maneuver
#'
#'@param v_start Velocity at the start of the maneuver (m/s)
#'@param v_end Velocity at the end of the maneuver (m/s)
#'@param c_decel Deceleration constant of the vehicle (m/s^2)

f_decel_time<-function(v_start,v_end,c_decel){(v_start-v_end)/c_decel}

#'@section Deceleration distance in m:
#'
#'@param v_start Velocity at the start of the maneuver (m/s)
#'#'@param v_end Velocity at the end of the maneuver (m/s)
#'@param c_decel Deceleration constant of the vehicle (m/s^2)

f_decel_distance <- function(v_start, v_end, c_decel )
  {v_start*f_decel_time(v_start, v_end, c_decel)+1/2*c_decel*f_ecel_time(v_start, v_end, c_decel)^2}

#'@section Corner distance in m:
#'
#'@param r_corner Radius of the corner (m)
#'@param corner_angle Angle of the corner (degrees)

f_corner_distance <- function(r_corner,corner_angle){(corner_angle/(360)*2*r_corner*pi)}


#### Slip Functions ####
#' Slip Functions
#' 
#' These functions are used to calculate slip. The functions include f_normal_load_force, 
#' f_mu_slip, f_mu_max, f_x_slip_long_force, f_slip_wheelspin, f_c_full_brake, 
#' f_slip_brake, f_mu_lat_slip and f_slip_lateral. 
#' 
### Slip caused by braking
# There are three scenarios in which the brakes are used, which are:
# i) the maximum brake test of the tyre, 
# ii) brake to decelerate 
# iii) brake to remain under speed limit at downhill slopes

## The slip [unitless or (m/s)/(m/s)] caused by braking is calculated
## by dividing the brake force needed to perform the maneuver with the maximum brake force the tyres can achieve

#'@section Brake slip during deceleration maneuvers
#' Brake slip during deceleration maneuvers is calculated by 
#' #'dividing the brake force needed upon the deceleration maneuver with the maximum brake force the tyres can achieve
#'

f_decel_brake_slip <- function (c_decel, m_vehicle, m_rotate, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope, c_max_brake)
{pmin(1, (f_decel_brake_force(c_decel, m_vehicle, m_rotate, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope) * 
    1/(f_max_brake_force(m_vehicle, m_rotate, c_max_brake)))) }

#'@section Brake slip during downhill constant speed driving
#' Brake slip during downhill constant speed driving is calculated by dividing the needed brake force to remain under speed limit with 
#' with the maximum brake force the tyres can achieve

f_downhill_brake_slip <- function (m_vehicle, grav_constant, alpha_slope, c_roll, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, m_rotate, c_max_brake)
{pmin(1,(f_downhill_brake_force(m_vehicle, grav_constant, alpha_slope, c_roll, c_drag, A_vehicle, rho_air, v_vehicle, v_wind) * 
    1/ (f_max_brake_force(m_vehicle, m_rotate, c_max_brake))))}

### Wheelspin slip

#'Wheelspin slip [unitless or (m/s)/(m/s)] is caused by the difference in forward velocity between the vehicle and the wheels, 
#'i.e. the wheels turn faster than the vehicle drives forward.
#'The vehicle maneuvers are all assumed to be in the low slip regime.
#'As such, wheelspin slip is estimated to be linear proportional to the friction coefficient. 
#'The friction coefficient is estimated by dividing the normal load force (N) acting on the vehicle with the total longitudinal resistant forces (N).
#'However this friction coefficient is limited to a maximum. 
#'This peak friction coefficient is estimated from the wet tyre grip index and corrected for the track underground.
#'The optimal slip ratio is reached in case the friction coefficient equals the maximum friction coefficient
#'Optimal slip ratio is parameter that is given as a characteristic of the road surface
#'  
#'@section Friction coefficients per maneuver (unitless):
#'Friction coefficients are calculated by dividing the longitudinal friction forces with the normal load force acting on the tyres
#'The longitudinal friction forces differ for acceleration, constant speed and deceleration maneuvers, 
#'so that the friction coefficients differ for these maneuver as well.
#'The functions for the friction coefficients (mu) are therefore estimated separately for acceleration, deceleration and constant speed driving

#'@'section Friction coefficient at acceleration
#'
#'The friction coefficient during acceleration is calculated by dividing the longitudinal force (N) during acceleration

f_accel_mu_slip <- function(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel)
{f_accel_force(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel)*
    1/(f_normal_load_force(alpha_slope, m_vehicle, grav_constant))}
 
#'The friction coefficient during constant speed driving is calculated by dividing the longitudinal force (N) during constant speed driving
#'by the normal load force (N)

f_constant_speed_mu_slip <- function(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope)
{(f_roll_force(c_roll, m_vehicle, grav_constant)+
    f_drag_force(c_drag, A_vehicle, rho_air, v_vehicle, v_wind)+
    pmax(0,f_slope_force(m_vehicle,grav_constant, alpha_slope)))*
    1/(f_normal_load_force(alpha_slope, m_vehicle, grav_constant))}

#'The friction coefficient during deceleration is calculated by dividing the longitudinal force (N) during deceleration
#'by the normal load force (N)

f_mu_decel_slip <- function(m_vehicle, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope)
{f_decel_resist_force(m_vehicle, m_rotate, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope)*
    1/(f_normal_load_force(alpha_slope, m_vehicle, grav_constant))}

#'@section Peak friction coefficient (unitless):
#'The peak friction coefficient (mu_max_tyre) is considered to be a characteristic of the tyres. 
#'On wet asphalt the peak friction coefficient(wet_mu_max_tyre) can be derived from the wet grip index number 
#'and the peak friction coefficient at wet asphalt of a reference tyre (wet_mu_max_ref_tyre) which has a value of 0.85.

f_wet_mu_max_tyre <-  function(grip_index_tyre, wet_mu_max_ref_tyre)
  {(grip_index_tyre * wet_mu_max_ref_tyre) / 1.25} 

# The peak friction coefficient between the tyre and the track is then estimated by 
# multiplying the wet peak friction coefficient on asphalt with a correction factor

f_mu_max_tyre_track <- function (grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track)
{f_wet_mu_max_tyre(grip_index_tyre, wet_mu_max_ref_tyre) * x_correct_mu_max_track}

# The wheelspin slip is then estimated per maneuver as (mu_maneuver/mu_max)*optimal_slip 

f_accel_wheelspin_slip <- function (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, 
                                    v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel, 
                                    grip_index_tyre, wet_mu_max_ref_tyre, 
                                    x_correct_mu_max_track, optimal_slip_ratio_track)
{f_accel_mu_slip (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel)*
    1/(f_mu_max_tyre_track(grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track))* 
    optimal_slip_ratio_track} 

f_decel_wheelspin_slip <- function(m_vehicle, c_roll, grav_constant, rho_air, 
                                   v_start_decel, v_end_decel, v_wind, alpha_slope, 
                                   grip_index_tyre, wet_mu_max_ref_tyre, 
                                   x_correct_mu_max_track, optimal_slip_ratio_track)
{f_mu_decel_slip(m_vehicle, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope)* 
    1/(f_mu_max_tyre_track(grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track))* 
    optimal_slip_ratio_track}

f_constant_speed_wheelspin_slip <- function (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope,
                                             grip_index_tyre, wet_mu_max_ref_tyre, 
                                             x_correct_mu_max_track, optimal_slip_ratio_track)
{f_constant_speed_mu_slip(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope)*
    1/(f_mu_max_tyre_track(grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track))* optimal_slip_ratio_track}

# Total slip 

# The slip per maneuver is estimated as the sum of wheelspin and brake slip

# Total slip during acceleration

f_accel_slip <- function (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, 
                          v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel, 
                          grip_index_tyre, wet_mu_max_ref_tyre, 
                          x_correct_mu_max_track, optimal_slip_ratio_track)
{f_accel_wheelspin_slip(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel, v_end_accel, 
                          v_wind, alpha_slope, m_rotate, c_accel, 
                        grip_index_tyre, wet_mu_max_ref_tyre, 
                        x_correct_mu_max_track, optimal_slip_ratio_track)+0}

# Total slip during deceleration


f_decel_slip <- function (m_vehicle, c_roll, grav_constant, rho_air, v_start_decel, v_end_decel, v_wind, alpha_slope, grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track, optimal_slip_ratio_track, 
                          c_decel, c_max_brake)
{f_decel_wheelspin_slip(m_vehicle, c_roll, grav_constant, rho_air, 
                        v_start_decel, v_end_decel, v_wind, alpha_slope, 
                        grip_index_tyre, wet_mu_max_ref_tyre, 
                        x_correct_mu_max_track, optimal_slip_ratio_track) + 
    f_decel_brake_slip(c_decel, m_vehicle, m_rotate, c_roll, grav_constant, rho_air, 
                       v_start_decel, v_end_decel, v_wind, alpha_slope, c_max_brake)}

# Total slip at constant speed

f_constant_speed_slip <- function (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope,
                                   grip_index_tyre, wet_mu_max_ref_tyre, 
                                   x_correct_mu_max_track, optimal_slip_ratio_track)
{f_constant_speed_wheelspin_slip(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope,
                                 grip_index_tyre, wet_mu_max_ref_tyre, 
                                 x_correct_mu_max_track, optimal_slip_ratio_track, m_rotate, c_max_brake) + 
    f_downhill_brake_slip(m_vehicle, grav_constant, alpha_slope, c_roll, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, m_rotate, c_max_brake)}

### Latitudinal slip functions

# Friction coefficient in latitudinal direction

f_lat_mu_slip <- function (m_vehicle , v_vehicle , r_corner, grav_constant, alpha_bank_slope)
{f_latitude_force(m_vehicle , v_vehicle , r_corner, grav_constant, alpha_bank_slope)* 
    1/(f_lat_normal_load_force(alpha_bank_slope,m_vehicle, grav_constant))}

## The linearity between slip and latitudinal force is 7 times larger than the linearity between slip and longitudinal forces

f_lat_slip <- function (m_vehicle , v_vehicle , r_corner, grav_constant, alpha_bank_slope, grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track, optimal_ratio_slip_track, x_linear_lat_vs_long_slip)
{f_lat_mu_slip(m_vehicle , v_vehicle , r_corner, grav_constant, alpha_bank_slope)*
    1/(f_mu_max_tyre_track(grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track))*
    x_linear_lat_vs_long_slip*optimal_ratio_slip_track}

