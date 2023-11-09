f_dec_dry <- function(sector_velocity_ms,
                      sector_end_velocity_ms,
                      c_decel_maneuver_2 = c_decel_comfort,
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
                      x_slip_long_force_dry_asphalt){
  maneuver_2 = "deceleration on dry asphalt"
  
  v_start_deceleration_maneuver_2 = sector_velocity_ms
  v_end_deceleration_maneuver_2 = pmin(sector_velocity_ms, 
                                       sector_end_velocity_ms) # Can these functions be merged?
  v_average_maneuver_2 = (v_start_deceleration_maneuver_2 + 
                            v_end_deceleration_maneuver_2)/2
  drag_force_t0_maneuver_2 = f_drag_force(c_drag, A_vehicle, 
                                          rho_air, 
                                          v_vehicle = v_start_deceleration_maneuver_2, 
                                          v_wind)
  drag_force_average_maneuver_2 = f_drag_force(c_drag, 
                                               A_vehicle, 
                                               rho_air, 
                                               v_vehicle = v_average_maneuver_2, 
                                               v_wind)
  roll_force_maneuver_2 = f_roll_force(c_roll, 
                                       m_vehicle, 
                                       grav_constant)
  slope_force_maneuver_2 = f_slope_force(m_vehicle = m_vehicle, 
                                         grav_constant = grav_constant, 
                                         alpha_slope = sector_alpha_slope)
  inert_decel_force_t0_maneuver_2 = (drag_force_t0_maneuver_2 + 
                                       roll_force_maneuver_2 + 
                                       pmax(0,slope_force_maneuver_2)) # What about negative slopes? HOw is that dealt with?
  c_decel_inert_t0_maneuver_2 = f_c_decel_inert(Decel_force = inert_decel_force_t0_maneuver_2, 
                                                m_vehicle = m_vehicle, 
                                                m_rotate = m_rotate) # waarom wordt dit uitgerekend (nergens gebruikt?)
  
  ## Brake force needed to descell comfortable
  
  brake_force_maneuver_2 = pmax(0,
                                (m_vehicle + m_rotate) * c_decel_maneuver_2 - 
                                  drag_force_average_maneuver_2 - 
                                  roll_force_maneuver_2 - 
                                  slope_force_maneuver_2)
  
  c_comfort_brake_maneuver_2 = brake_force_maneuver_2 * 1/(m_vehicle + m_rotate)
  
  slip_brake_maneuver_2 = f_slip_brake(c_decel_brake =c_comfort_brake_maneuver_2, c_full_brake = c_full_brake_dry_asphalt)
  
  slip_maneuver_2= slip_brake_maneuver_2 + f_slip_wheelspin(optimal_slip = optimal_slip_dry_asphalt, 
                                                            x_slip_long_force = x_slip_long_force_dry_asphalt, 
                                                            long_force = drag_force_average_maneuver_2 + 
                                                              roll_force_maneuver_2 + 
                                                              pmin(0,slope_force_maneuver_2))
  
  
  duration_maneuver_2 = f_decel_time(v_start = v_start_deceleration_maneuver_2, 
                                     v_end = v_end_deceleration_maneuver_2, 
                                     c_decel = c_decel_maneuver_2)
  distance_maneuver_2 = f_decel_distance(v_start = v_start_deceleration_maneuver_2, 
                                         Decel_time = duration_maneuver_2 , 
                                         c_decel = c_decel_maneuver_2)
  
  longitude_friction_work_maneuver_2 = f_longitude_friction_work(long_force = (drag_force_average_maneuver_2 + 
                                                                                 roll_force_maneuver_2 + 
                                                                                 pmax(0,slope_force_maneuver_2) + 
                                                                                 brake_force_maneuver_2), 
                                                                 slip = slip_maneuver_2, 
                                                                 distance = distance_maneuver_2)                    
  
  
  list(maneuver=maneuver_2,distance=distance_maneuver_2, friction_work_longitude=longitude_friction_work_maneuver_2)
  
}