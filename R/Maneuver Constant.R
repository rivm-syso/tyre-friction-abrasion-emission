#' Constant maneuver 
#' 
#' This function is used to calculate the distance in m and longitudinal friction work in N 
#' of a constant driving maneuver on dry asphalt. These values are returned in a list. 
#'
#'@param sector_velocity_ms Vehicle velocity at the middle of the sector (m/s)
#'@param m_vehicle Mass of the vehicle (kg)
#'@param c_drag Drag coefficient of the vehicle (unitless)
#'@param A_vehicle Vehicle frontal area (m^2)
#'@param rho_air Density of air (kg/m^3)
#'@param v_wind Wind velocity (m/s)
#'@param c_roll Roll coefficient of the tyre (kg/kg)
#'@param grav_constant Gravitational constant (m/s^2)
#'@param sector_alpha_slope Slope of the sector (%)
#'@param c_full_brake_dry_asphalt Deceleration constant of the tyre at full wheel lock on dry asphalt (m/s^2)
#'@param optimal_slip_dry_asphalt Slip ratio between tyre and track (unitless)
#'@param x_slip_long_force_dry_asphalt Longitudinal slip force on dry asphalt (N)
#'@param distance_maneuver_3 Constant driving distance (m)

f_const_dry <- function(sector_velocity_ms,  
                        m_vehicle,
                        m_rotate,
                        c_drag,
                        A_vehicle,
                        rho_air,
                        v_wind,
                        c_roll,
                        grav_constant,
                        sector_alpha_slope,
                        c_full_brake_dry_asphalt,
                        optimal_slip_dry_asphalt,
                        x_slip_long_force_dry_asphalt,
                        distance_maneuver_3 ){
  
  # Longitudinal friction work during constant speed driving
  maneuver_3 = "constant speed driving"
  
  # Constant speed driving is the maneuver at which the vehicle is driving over the track sector with a constant velocity.
  #The longitudinal resistive forces during this maneuver are 
  # the roll force
  roll_force_maneuver_3 = f_roll_force(c_roll, 
                                       m_vehicle, 
                                       grav_constant)
  # the drag force 
  drag_force_maneuver_3 = f_drag_force(c_drag, 
                                       A_vehicle, 
                                       rho_air, 
                                       v_vehicle = sector_velocity_ms, 
                                       v_wind)
  
  # the slope force (in case of uphill driving)
  slope_force_maneuver_3 = f_slope_force(m_vehicle = m_vehicle, 
                                         grav_constant = grav_constant, 
                                         alpha_slope = sector_alpha_slope)
  
  # the brake force (in case downhill driving downhill using the brakes remain under the speed limit)
  brake_force_maneuver_3 = pmax(0,-(drag_force_maneuver_3 + roll_force_maneuver_3 +slope_force_maneuver_3)) 
  
  # Slip during constant speed driving on dry asphalt
  c_decel_brake_maneuver_3 = pmin(c_full_brake_dry_asphalt , 
                                  f_c_decel_inert(Decel_force = brake_force_maneuver_3, 
                                                  m_vehicle = m_vehicle, 
                                                  m_rotate = m_rotate))
  
  # slip ratio of brake maneuver 
  slip_brake_maneuver_3 = f_slip_brake(c_decel_brake = c_decel_brake_maneuver_3, 
                                       c_full_brake = c_full_brake_dry_asphalt)
  
  # 
  slip_maneuver_3 = pmin(1,
                         f_slip_wheelspin(optimal_slip = optimal_slip_dry_asphalt, 
                                          x_slip_long_force = x_slip_long_force_dry_asphalt, 
                                          long_force = drag_force_maneuver_3 + 
                                            roll_force_maneuver_3 + 
                                            pmin(0,slope_force_maneuver_3)) + 
                           slip_brake_maneuver_3)
  
  
  # duration of the maneuver
  duration_maneuver_3 = distance_maneuver_3*sector_velocity_ms
  
  # longitude friction work
  longitude_friction_work_maneuver_3 = f_longitude_friction_work(long_force = (drag_force_maneuver_3 + 
                                                                                 roll_force_maneuver_3 + 
                                                                                 pmax(0,slope_force_maneuver_3) + 
                                                                                 brake_force_maneuver_3), 
                                                                 slip = slip_maneuver_3, 
                                                                 distance = distance_maneuver_3)
  
  # create a list containing the name, distance and friction work of the maneuver
  list(maneuver = maneuver_3, distance = distance_maneuver_3, friction_work_longitude = longitude_friction_work_maneuver_3) 
}