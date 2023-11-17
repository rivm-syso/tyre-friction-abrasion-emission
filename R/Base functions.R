#' Friction Force Functions
#' 
#' These functions are used to calculate the friction force. The functions include
#' f_roll_force, f_drag_force, f_slope_force, f_brake_force, f_accel_force, f_c_decel_inert, 
#' f_centripet_force and f_bank_force. 
#'
#'@section Roll resistance force in N:
#'
#'@param c_roll the roll coefficient of the tyre (kg/kg)
#'@param m_vehicle mass of the vehicle (kg)
#'@param grav_constant gravitational constant

f_roll_force <- function(c_roll, m_vehicle, grav_constant) {c_roll*m_vehicle*grav_constant}

#'@section Drag resistance force in N:
#'
#'@param c_drag drag coefficient of the vehicle (unitless)
#'@param rho_air density of air (km/m^3)
#'@param v_vehicle velocity of driving maneuver (m/s)
#'@param v_wind wind velocity (m/s)

f_drag_force <- function(c_drag, A_vehicle, rho_air, v_vehicle, v_wind) {pmax(0,c_drag*A_vehicle*rho_air*(((v_vehicle)+v_wind)^2)/2)}

#'@section Slope force in N:
#'@param m_vehicle mass of the vehicle (kg)
#'@param grav_constant gravitational constant
#'@param alpha_slope slope of the road in longitudinal direction (degrees)

f_slope_force <- function(m_vehicle, grav_constant, alpha_slope) {m_vehicle*grav_constant*sin(alpha_slope)}

#'@section Brake force in N:
#'
#'@param m_vehicle mass of the vehicle (kg)
#'@param m_rotate mass of the rotating parts (kg)
#'@param c_brake breaking constant of the vehicle (m/s^2)  

f_brake_force <- function(m_vehicle, m_rotate, c_brake) {(m_vehicle+m_rotate)*c_brake}

#'@section Inertia force upon acceleration in N: 
#'
#'@param m_vehicle mass of the vehicle (kg)
#'@param m_rotate mass of the rotating parts (kg)
#'@param c_accel acceleration constant of the vehicle (m/s^2) 

f_accel_force <- function(m_vehicle, m_rotate, c_accel) {(m_vehicle+m_rotate)*c_accel}

#'@section Inertia force upon deceleration (laying foot off gas) in N:
#'
#'@param Decel_force deceleration force (N)
#'@param m_vehicle mass of the vehicle (kg)
#'@param m_rotate mass of the rotating parts (kg)

f_c_decel_inert <- function(Decel_force,m_vehicle,m_rotate){Decel_force/(m_vehicle+m_rotate)}

#'@section Centripetal force in cornering in N:
#'
#'@param m_vehicle mass of the vehicle (kg)
#'@param v_vehicle velocity of the vehicle (m/s)
#'@param r_corner radius of the corner (m)
f_centripet_force <- function(m_vehicle , v_vehicle , r_corner) {m_vehicle*(v_vehicle^2)/r_corner}

#'@section Bank force in cornering in N:
#'
#'@param grav_constant gravitational constant 
#'@param apha_bank_slope bank slope of the road in latitudinal direction (degrees)
#'@param m_vehicle mass of the vehicle (kg)

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
#'@param v_start velocity at the start of the maneuver (m/s)
#'@param v_end velocity at the end of the maneuver (m/s)
#'@param c_accel acceleration constant of the vehicle (m/s^2) 

f_accel_time<-function(v_start,v_end,c_accel){-(v_start-v_end)/c_accel}

#'@section Acceleration distance in m:
#'@param v_start velocity at the start of the maneuver (m/s)
#'@param Accel_time acceleration time (s)
#'@param c_accel acceleration constant of the vehicle (m/s^2) 

f_accel_distance <- function(v_start , Accel_time , c_accel ) {v_start*Accel_time+1/2*c_accel*Accel_time^2}

#'@section Deceleration time in s:
#'
#'@param v_start = velocity at the start of the maneuver (m/s)
#'@param v_end = velocity at the end of the maneuver (m/s)
#'@param c_decel = deceleration constant of the vehicle (m/s^2)

f_decel_time<-function(v_start,v_end,c_decel){(v_start-v_end)/c_decel}

#'@section Deceleration distance in m:
#'
#'@param v_start = velocity at the start of the maneuver (m/s)
#'@param Decel_time = deceleration time (s)
#'@param c_decel = deceleration constant of the vehicle (m/s^2)

f_decel_distance <- function(v_start , Decel_time , c_decel ) {v_start*Decel_time+1/2*c_decel*Decel_time^2}

#'@section Longitude friction work in N:
#'
#'@param long_force = longitudinal force (N)
#'@param slip = difference between the actual vehicle velocity and radiant velocity of the wheels (m/s)
#'@param distance = distance of maneuver (m)

f_longitude_friction_work <- function(long_force , slip, distance){(long_force)*slip*distance}

#'@section Latitude friction work in N:
#'@param lat_force latitudinal force (N) 
#'@param slip difference between the actual vehicle velocity and radiant velocity of the wheels (m/s)
#'@param distance distance of maneuver (m)

f_latitude_friction_work <- function(lat_force, slip, distance){lat_force*slip*distance}

#'@section Corner distance in m:
#'@param r_corner radius of the corner (m)
#'@param corner_angle angle of the corner (degrees)

f_corner_distance <- function(r_corner,corner_angle){(corner_angle/(360)*2*r_corner*pi)}

#### Slip Functions ####
#'@section Normal load force in N:
#'@param m_vehicle mass of the vehicle (kg)
#'@param grav_constant gravitational constant 

f_normal_load_force <- function(m_vehicle, grav_constant){m_vehicle*grav_constant}

#'@section Slip friction coefficient (unitless):
#'@param normal_load_force normal load force (N)
#'@param long_force longitudinal force (N)

f_mu_slip <- function(normal_load_force, long_force){normal_load_force/long_force}

#'@section Maximum friction coefficient (unitless):
#'@param grip_index_tyre tyre grip index (unitless)
#'@param x_correct_road correction factor for wet to dry or wet to wet conditions (unitless)
#'@param mu_max_ref_tyre Peak friction coefficient of EU reference tyre on EU reference track 
f_mu_max <- function(grip_index_tyre, x_correct_road, mu_max_ref_tyre){(grip_index_tyre)/((1/mu_max_ref_tyre)*(125/100))*x_correct_road}

#'@section Longitude slip force in N
#'@param mu_max Maximum friction coefficient (unitless)
#'@param normal_load_force Normal load force (N)
#'@param optimal_slip Optimal slip ratio between tyre and track (unitless)

f_x_slip_long_force <- function(mu_max, normal_load_force, optimal_slip){(optimal_slip)/(mu_max*normal_load_force)}

#'@section Wheelspin (m/s?):
#'@param optimal_slip Optimal slip ratio between tyre and track (unitless)
#'@param x_slip_long_force 
#'@param long_force Longitudinal force (N)

f_slip_wheelspin <- function (optimal_slip, x_slip_long_force, long_force){pmin(optimal_slip, x_slip_long_force*long_force)} 

#'@param grip_index_tyre Tyre grip index
#'@param x_correct_road correction factor for wet to dry or wet to wet conditions (unitless)
#'@param c_full_brake_ref_tyre Brake deceleration constant of EU reference tyre on EU reference track (??)

f_c_full_brake <-function(grip_index_tyre,
                          x_correct_road,
                          c_full_brake_ref_tyre){
  (grip_index_tyre)/((1/c_full_brake_ref_tyre)*(125/100))*x_correct_road
}

#'@section Slip ratio during brake maneuvers
#'@param c_decel_brake 
#'@param c_full_brake Deceleration constant of the tyre at full wheel lock (m/s^2)

f_slip_brake <- function (c_decel_brake, c_full_brake){c_decel_brake/c_full_brake}

#'@section 
#'@param normal_load_force Normal load force (N)
#'@param long_force Longitudinal force (N)
f_mu_lat_slip <- function(normal_load_force, long_force){normal_load_force/lat_force} 

#'@section Latitudinal slip 
#'@param optimal_slip Optimal slip ratio between tyre and track (unitless)
#'@param x_slip_lat_force 
#'@param lat_force Lateral force (N)

f_slip_lateral <- function (optimal_slip, x_slip_lat_force, lat_force){pmin(optimal_slip, x_slip_lat_force*lat_force)}
