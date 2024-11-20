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
  ## Constant speed driving is the maneuver at which the vehicle is driving over the track sector with a constant velocity.
  ##The longitudinal resistive forces during this maneuver are 
  ## the roll force
  roll_force_maneuver_3 = f_roll_force(c_roll, 
                                       m_vehicle, 
                                       grav_constant)
  ## the drag force 
  drag_force_maneuver_3 = f_drag_force(c_drag, 
                                       A_vehicle, 
                                       rho_air, 
                                       v_vehicle = sector_velocity_ms, 
                                       v_wind)
  ## the slope force (in case of uphill driving)
  slope_force_maneuver_3 = f_slope_force(m_vehicle = m_vehicle, 
                                         grav_constant = grav_constant, 
                                         alpha_slope = sector_alpha_slope)
  ## the brake force (in case downhill driving downhill using the brakes remain under the speed limit)
  brake_force_maneuver_3 = pmax(0,-(drag_force_maneuver_3 + roll_force_maneuver_3 +slope_force_maneuver_3)) 
  
  ## Slip during constant speed driving on dry asphalt
  c_decel_brake_maneuver_3 = pmin(c_full_brake_dry_asphalt , 
                                  f_c_decel_inert(Decel_force = brake_force_maneuver_3, 
                                                  m_vehicle = m_vehicle, 
                                                  m_rotate = m_rotate))
  slip_brake_maneuver_3 = f_slip_brake(c_decel_brake = c_decel_brake_maneuver_3, 
                                       c_full_brake = c_full_brake_dry_asphalt)
  slip_maneuver_3 = pmin(1,
                         f_slip_wheelspin(optimal_slip = optimal_slip_dry_asphalt, 
                                          x_slip_long_force = x_slip_long_force_dry_asphalt, 
                                          long_force = drag_force_maneuver_3 + 
                                            roll_force_maneuver_3 + 
                                            pmin(0,slope_force_maneuver_3)) + 
                           slip_brake_maneuver_3)
  
  
  
  duration_maneuver_3 = distance_maneuver_3*sector_velocity_ms
  longitude_friction_work_maneuver_3 = f_longitude_friction_work(long_force = (drag_force_maneuver_3 + 
                                                                                 roll_force_maneuver_3 + 
                                                                                 pmax(0,slope_force_maneuver_3) + 
                                                                                 brake_force_maneuver_3), 
                                                                 slip = slip_maneuver_3, 
                                                                 distance = distance_maneuver_3)
  list(maneuver=maneuver_3,
       distance=distance_maneuver_3,
       friction_work_longitude=longitude_friction_work_maneuver_3) 
}