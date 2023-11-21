#' Acceleration maneuver 
#' 
#' This function is used to calculate the distance in m and friction work in N 
#' of an acceleration maneuver on dry asphalt. These values are returned in a list. 
#' 
#' @param sector_velocity_ms Vehicle velocity at the middle of the sector (m/s)
#' @param sector_start_velocity_ms Vehicle velocity at the start of the sector (m/s)
#' @param frac_driver_comfort_max_accelaration Comfortable acceleration constant (unitless)
#' @param c_accel_max Maximum acceleration 
#' @param m_vehicle Mass of the vehicle (kg)
#' @param m_rotate Mass of the rotating parts (kg)
#' @param c_drag Drag coefficient of the vehicle (unitless)
#' @param A_vehicle Vehicle frontal area (m^2)
#' @param rho_air Density of air (kg/m^3) 
#' @param v_wind Wind velocity (m/s)
#' @param c_roll Roll coefficient of the tyre (kg/kg)
#' @param grav_constant Gravitational constant (m/s^2)
#' @param sector_alpha_slope Slope of the sector (%)
#' @param optimal_slip_dry_asphalt Optimal slip ratio between tyre and track (unitless)
#' @param x_slip_long_force_dry_asphalt Longitudinal slip force on dry asphalt (N)
#' @param accel_force_end_maneuver_1 ???

f_acc_dry <- function(sector_velocity_ms,
                      sector_start_velocity_ms,
                      frac_driver_comfort_max_accelaration,
                      c_accel_max,
                      m_vehicle,
                      m_rotate,
                      c_drag,
                      A_vehicle,
                      rho_air,
                      v_wind,
                      c_roll,
                      grav_constant,
                      sector_alpha_slope,
                      optimal_slip_dry_asphalt,
                      x_slip_long_force_dry_asphalt,
                      accel_force_end_maneuver_1 = 0){
  
  
  maneuver_1 = "acceleration on dry asphalt"
  
  v_start_acceleration_maneuver_1 = pmin(sector_velocity_ms,sector_start_velocity_ms) 
  
  v_end_acceleration_maneuver_1 = sector_velocity_ms
  
  v_average_maneuver_1 = (v_start_acceleration_maneuver_1 + v_end_acceleration_maneuver_1)/2
  
  c_accel_maneuver_1 <- frac_driver_comfort_max_accelaration * c_accel_max
  
  accel_force_t0_maneuver_1 = f_accel_force(m_vehicle = m_vehicle, 
                                            m_rotate = m_rotate, 
                                            c_accel = c_accel_maneuver_1)
  
  
  accel_force_average_maneuver_1 = (accel_force_t0_maneuver_1 + accel_force_end_maneuver_1)/2
  
  duration_maneuver_1 = f_accel_time(v_start = v_start_acceleration_maneuver_1, 
                                     v_end = v_end_acceleration_maneuver_1, 
                                     c_accel = c_accel_maneuver_1)
  distance_maneuver_1 = f_accel_distance(v_start = v_start_acceleration_maneuver_1, 
                                         Accel_time = duration_maneuver_1, 
                                         c_accel = c_accel_maneuver_1)
  
  drag_force_maneuver_1 = f_drag_force(c_drag = c_drag, 
                                       A_vehicle = A_vehicle, 
                                       rho_air = rho_air, 
                                       v_vehicle = v_average_maneuver_1, 
                                       v_wind = v_wind)
  roll_force_maneuver_1 = f_roll_force(c_roll=c_roll , 
                                       m_vehicle = m_vehicle, 
                                       grav_constant = grav_constant) 
  
  # so here you implement to take 0 if the slope is going downhill for the force related to the slope. 
  # What about the reduced need to accelerate under a negative slope? This does not seem to be implemented.
  slope_force_maneuver_1 = pmax(0,f_slope_force(m_vehicle = m_vehicle, 
                                                grav_constant = grav_constant, 
                                                alpha_slope = sector_alpha_slope))
  
  slip_maneuver_1 = f_slip_wheelspin(optimal_slip = optimal_slip_dry_asphalt, 
                                     x_slip_long_force = x_slip_long_force_dry_asphalt, 
                                     long_force = (accel_force_average_maneuver_1+ 
                                                     drag_force_maneuver_1+ 
                                                     roll_force_maneuver_1+ 
                                                     slope_force_maneuver_1)) # changed as pmin(0,slope_force_maneuver_1) is not expected and probably incorrect as it is always 0
  
  longitude_friction_work_maneuver_1 = f_longitude_friction_work(long_force = (drag_force_maneuver_1 + roll_force_maneuver_1 + slope_force_maneuver_1 + accel_force_average_maneuver_1), slip = slip_maneuver_1, distance = distance_maneuver_1)
  
  # this implementation results in relationship between distance of the this maneuver and the required time needed to go from starting velocity to end velocity. Care should be taken to then allow the rest of the track distance to be driven at constant speed.
  # for this reason a list is reported of both
  list(maneuver=maneuver_1,distance=distance_maneuver_1, friction_work_longitude = longitude_friction_work_maneuver_1)
}


