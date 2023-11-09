#### Friction force functions ####
# Roll resistance force in N
f_roll_force <- function(c_roll, m_vehicle, grav_constant) {c_roll*m_vehicle*grav_constant}

# Drag resistance force in N
f_drag_force <- function(c_drag, A_vehicle, rho_air, v_vehicle, v_wind) {pmax(0,c_drag*A_vehicle*rho_air*(((v_vehicle)+v_wind)^2)/2)}

# Slope force
f_slope_force <- function(m_vehicle, grav_constant, alpha_slope) {m_vehicle*grav_constant*sin(alpha_slope)}

# Brake force
f_brake_force <- function(m_vehicle, m_rotate, c_brake) {(m_vehicle+m_rotate)*c_brake}

# Inertia force upon acceleration
f_accel_force <- function(m_vehicle, m_rotate, c_accel) {(m_vehicle+m_rotate)*c_accel}

# Inertia force upon deceleration (laying foot of gas)
f_c_decel_inert <- function(Decel_force,m_vehicle,m_rotate){Decel_force/(m_vehicle+m_rotate)}

# Centripetal force in cornering
f_centripet_force <- function(m_vehicle , v_vehicle , r_corner) {m_vehicle*(v_vehicle^2)/r_corner}

# Bank force in cornering
f_bank_force <-function(grav_constant, alpha_bank_slope, m_vehicle){grav_constant*sin(alpha_bank_slope)*m_vehicle}

#### Distance functions ####

# Acceleration distance in m
# function of acceleration time
f_accel_time<-function(v_start,v_end,c_accel){-(v_start-v_end)/c_accel}

# function acceleration distance
f_accel_distance <- function(v_start , Accel_time , c_accel ) {v_start*Accel_time+1/2*c_accel*Accel_time^2}

# Deceleration distance in m
f_decel_time<-function(v_start,v_end,c_decel){(v_start-v_end)/c_decel}

f_decel_distance <- function(v_start , Decel_time , c_decel ) {v_start*Decel_time+1/2*c_decel*Decel_time^2}

f_longitude_friction_work <- function(long_force , slip, distance){(long_force)*slip*distance}
f_latitude_friction_work <- function(lat_force, slip, distance){lat_force*slip*distance}

# Corner distance in m
f_corner_distance <- function(r_corner,corner_angle){(corner_angle/(360)*2*r_corner*pi)}

### Slip Functions ###
f_normal_load_force <- function(m_vehicle, grav_constant){m_vehicle*grav_constant}
f_mu_slip <- function(normal_load_force, long_force){normal_load_force/long_force}
f_mu_max <- function(grip_index_tyre, x_correct_road, mu_max_ref_tyre){(grip_index_tyre)/((1/mu_max_ref_tyre)*(125/100))*x_correct_road}
f_x_slip_long_force <- function(mu_max, normal_load_force, optimal_slip){(optimal_slip)/(mu_max*normal_load_force)}
f_slip_wheelspin <- function (optimal_slip, x_slip_long_force, long_force){pmin(optimal_slip, x_slip_long_force*long_force)} 

f_c_full_brake <-function(grip_index_tyre,
                          x_correct_road,
                          c_full_brake_ref_tyre){
  (grip_index_tyre)/((1/c_full_brake_ref_tyre)*(125/100))*x_correct_road
}

f_slip_brake <- function (c_decel_brake, c_full_brake){c_decel_brake/c_full_brake}
f_mu_lat_slip <- function(normal_load_force, long_force){normal_load_force/lat_force} 
f_slip_lateral <- function (optimal_slip, x_slip_lat_force, lat_force){pmin(optimal_slip, x_slip_lat_force*lat_force)}
