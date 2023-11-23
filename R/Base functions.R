#' Friction Force Functions
#' 
#' These functions are used to calculate the friction force. The functions include
#' f_roll_force, f_drag_force, f_slope_force, f_brake_force, f_accel_force, f_c_decel_inert, 
#' f_centripet_force and f_bank_force. 
#'
#'@section Roll resistance force in N:
#'
#'@param c_roll The roll coefficient of the tyre (kg/kg)
#'@param m_vehicle Mass of the vehicle (kg)
#'@param grav_constant Gravitational constant (m/s^2)

f_roll_force <- function(c_roll, m_vehicle, grav_constant) {c_roll*m_vehicle*grav_constant}

#'@section Drag resistance force in N:
#'
#'@param c_drag Drag coefficient of the vehicle (unitless)
#'@param rho_air Density of air (km/m^3)
#'@param v_vehicle Velocity of driving maneuver (m/s)
#'@param v_wind Wind velocity (m/s)

f_drag_force <- function(c_drag, A_vehicle, rho_air, v_vehicle, v_wind) {pmax(0,c_drag*A_vehicle*rho_air*(((v_vehicle)+v_wind)^2)/2)}

#'@section Slope force in N:
#'
#'@param m_vehicle Mass of the vehicle (kg)
#'@param grav_constant Gravitational constant (m/s^2)
#'@param alpha_slope Slope of the road in longitudinal direction (degrees)

f_slope_force <- function(m_vehicle, grav_constant, alpha_slope) {m_vehicle*grav_constant*sin(alpha_slope)}

#'@section Brake force in N:
#'
#'@param m_vehicle Mass of the vehicle (kg)
#'@param m_rotate Mass of the rotating parts (kg)
#'@param c_brake Braking constant of the vehicle (m/s^2)  

f_brake_force <- function(m_vehicle, m_rotate, c_brake) {(m_vehicle+m_rotate)*c_brake}

#'@section Acceleration force in N: 
#'
#'@param m_vehicle Mass of the vehicle (kg)
#'@param m_rotate Mass of the rotating parts (kg)
#'@param c_accel Acceleration constant of the vehicle (m/s^2) 

f_accel_force <- function(m_vehicle, m_rotate, c_accel) {(m_vehicle+m_rotate)*c_accel}

#'@section Inertia force upon deceleration (laying foot off gas) in N:
#'
#'@param Decel_force Deceleration force (N)
#'@param m_vehicle Mass of the vehicle (kg)
#'@param m_rotate Mass of the rotating parts (kg)

f_c_decel_inert <- function(Decel_force,m_vehicle,m_rotate){Decel_force/(m_vehicle+m_rotate)}

#'@section Centripetal force in cornering in N:
#'
#'@param m_vehicle Mass of the vehicle (kg)
#'@param v_vehicle Velocity of the vehicle (m/s)
#'@param r_corner Radius of the corner (m)

f_centripet_force <- function(m_vehicle , v_vehicle , r_corner) {m_vehicle*(v_vehicle^2)/r_corner}

#'@section Bank force in cornering in N:
#'
#'@param grav_constant Gravitational constant (m/s^2) 
#'@param apha_bank_slope Bank slope of the road in latitudinal direction (degrees)
#'@param m_vehicle Mass of the vehicle (kg)

f_bank_force <-function(grav_constant, alpha_bank_slope, m_vehicle){grav_constant*sin(alpha_bank_slope)*m_vehicle}

#### Distance functions ####
#' Distance functions
#' 
#' These functions are used to calculate the distances over which driving maneuvers 
#' take place. The functions include f_accel_time, f_accel_distance, f_decel_time,
#' f_decel_distance, f_longitude_friction_work, f_latitude_friction_work and
#' f_corner_distance. 
#' 
#'@section Acceleration time in s: 
#'
#'@param v_start Velocity at the start of the maneuver (m/s)
#'@param v_end Velocity at the end of the maneuver (m/s)
#'@param c_accel Acceleration constant of the vehicle (m/s^2) 

f_accel_time<-function(v_start,v_end,c_accel){-(v_start-v_end)/c_accel}

#'@section Acceleration distance in m:
#'
#'@param v_start Velocity at the start of the maneuver (m/s)
#'@param Accel_time Acceleration time (s)
#'@param c_accel Acceleration constant of the vehicle (m/s^2) 

f_accel_distance <- function(v_start , Accel_time , c_accel ) {v_start*Accel_time+1/2*c_accel*Accel_time^2}

#'@section Deceleration time in s:
#'
#'@param v_start Velocity at the start of the maneuver (m/s)
#'@param v_end Velocity at the end of the maneuver (m/s)
#'@param c_decel Deceleration constant of the vehicle (m/s^2)

f_decel_time<-function(v_start,v_end,c_decel){(v_start-v_end)/c_decel}

#'@section Deceleration distance in m:
#'
#'@param v_start Velocity at the start of the maneuver (m/s)
#'@param Decel_time Deceleration time (s)
#'@param c_decel Deceleration constant of the vehicle (m/s^2)

f_decel_distance <- function(v_start , Decel_time , c_decel ) {v_start*Decel_time+1/2*c_decel*Decel_time^2}

#'@section Longitude friction work in N:
#'
#'@param long_force Longitudinal force (N)
#'@param slip Difference between the actual vehicle velocity and radiant velocity of the wheels (m/s)
#'@param distance Distance of maneuver (m)

f_longitude_friction_work <- function(long_force, slip, distance){(long_force)*slip*distance}

#'@section Latitude friction work in N:
#'
#'@param lat_force Latitudinal force (N) 
#'@param slip Difference between the actual vehicle velocity and radiant velocity of the wheels (m/s)
#'@param distance Distance of maneuver (m)

f_latitude_friction_work <- function(lat_force, slip, distance){lat_force*slip*distance}

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
#'@section Normal load force in N:
#'
#'@param m_vehicle Mass of the vehicle (kg)
#'@param grav_constant Gravitational constant (m/s^2)

f_normal_load_force <- function(m_vehicle, grav_constant){m_vehicle*grav_constant}

#'@section Slip friction coefficient (unitless):
#'
#'@param normal_load_force Normal load force (N)
#'@param long_force Longitudinal force (N)

f_mu_slip <- function(normal_load_force, long_force){normal_load_force/long_force}

#'@section Maximum friction coefficient (unitless):
#'
#'@param grip_index_tyre Tyre grip index (unitless)
#'@param x_correct_road Correction factor for wet to dry or wet to wet conditions (unitless)
#'@param mu_max_ref_tyre Peak friction coefficient of EU reference tyre on EU reference track (??)
f_mu_max <- function(grip_index_tyre, x_correct_road, mu_max_ref_tyre){(grip_index_tyre)/((1/mu_max_ref_tyre)*(125/100))*x_correct_road}

#'@section Longitude slip force in N
#'
#'@param mu_max Maximum friction coefficient (unitless)
#'@param normal_load_force Normal load force (N)
#'@param optimal_slip Optimal slip ratio between tyre and track (unitless)

f_x_slip_long_force <- function(mu_max, normal_load_force, optimal_slip){(optimal_slip)/(mu_max*normal_load_force)}

#'@section Wheelspin (m/s?):
#'
#'@param optimal_slip Optimal slip ratio between tyre and track (unitless)
#'@param x_slip_long_force Longitudinal slip force (N)
#'@param long_force Longitudinal force (N)

f_slip_wheelspin <- function (optimal_slip, x_slip_long_force, long_force){pmin(optimal_slip, x_slip_long_force*long_force)} 

#'@section Deceleration constant of the tyre at full wheel lock in m/s^2 
#'
#'@param grip_index_tyre Tyre grip index (unitless)
#'@param x_correct_road Correction factor for wet to dry or wet to wet conditions (unitless)
#'@param c_full_brake_ref_tyre Brake deceleration constant of EU reference tyre on EU reference track (??)

f_c_full_brake <-function(grip_index_tyre,
                          x_correct_road,
                          c_full_brake_ref_tyre){
  (grip_index_tyre)/((1/c_full_brake_ref_tyre)*(125/100))*x_correct_road
}

#'@section Slip ratio during brake maneuvers (unitless):
#'
#'@param c_decel_brake ?? (m/s^2)
#'@param c_full_brake Deceleration constant of the tyre at full wheel lock (m/s^2)

f_slip_brake <- function (c_decel_brake, c_full_brake){c_decel_brake/c_full_brake}

#'@section Latitudinal slip friction coefficient (unitless):
#'
#'@param normal_load_force Normal load force (N)
#'@param long_force Longitudinal force (N)
#'
f_mu_lat_slip <- function(normal_load_force, long_force){normal_load_force/lat_force} 

#'@section Lateral slip (??)
#'
#'@param optimal_slip Optimal slip ratio between tyre and track (unitless)
#'@param x_slip_lat_force Lateral slip force (N)
#'@param lat_force Lateral force (N)

f_slip_lateral <- function (optimal_slip, x_slip_lat_force, lat_force){pmin(optimal_slip, x_slip_lat_force*lat_force)}
