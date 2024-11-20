f_corn_dry <- function(sector_velocity_ms,
                       sector_corner_radius,
                       m_vehicle,
                       m_rotate,
                       sector_bank_slope,
                       sector_corner_angle,
                       c_drag,
                       A_vehicle,
                       rho_air,
                       v_wind,
                       c_roll,
                       sector_alpha_slope,
                       grav_constant,
                       c_full_brake_dry_asphalt,
                       optimal_slip_dry_asphalt,
                       x_slip_long_force_dry_asphalt){  
  # latitudinal friction work during cornering
  maneuver_4 = "cornering"
  centripet_force_maneuver_4 = f_centripet_force(m_vehicle = m_vehicle, 
                                                 v_vehicle = sector_velocity_ms, 
                                                 r_corner = sector_corner_radius)
  bank_force_maneuver_4 = f_bank_force(grav_constant = grav_constant,
                                       alpha_bank_slope = sector_bank_slope, 
                                       m_vehicle = m_vehicle)
  slip_lateral_maneuver_4 = f_slip_lateral(optimal_slip = optimal_slip_dry_asphalt, 
                                           x_slip_lat_force = 7*x_slip_long_force_dry_asphalt, 
                                           lat_force = centripet_force_maneuver_4 + 
                                             bank_force_maneuver_4)
  distance_maneuver_4 = f_corner_distance(corner_angle = sector_corner_angle , 
                                          r_corner = sector_corner_radius)
  latitude_friction_work_maneuver_4 = f_latitude_friction_work(lat_force = centripet_force_maneuver_4 + 
                                                                 bank_force_maneuver_4, 
                                                               slip = slip_lateral_maneuver_4, 
                                                               distance = distance_maneuver_4)
  
  
  drag_force_maneuver_3 = f_drag_force(c_drag, 
                                       A_vehicle, 
                                       rho_air, 
                                       v_vehicle = sector_velocity_ms, 
                                       v_wind)
  roll_force_maneuver_3 = f_roll_force(c_roll, 
                                       m_vehicle, 
                                       grav_constant)
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
  
  # longitude friction work during cornering
  longitude_friction_work_maneuver_4 = f_longitude_friction_work(long_force = (drag_force_maneuver_3 + 
                                                                                 roll_force_maneuver_3 + 
                                                                                 pmax(0,slope_force_maneuver_3) + 
                                                                                 brake_force_maneuver_3), 
                                                                 slip = slip_maneuver_3, 
                                                                 distance = distance_maneuver_4)
  
  list(maneuver=maneuver_4,
       distance=distance_maneuver_4,
       friction_work_longitude = longitude_friction_work_maneuver_4,
       friction_work_latitude = latitude_friction_work_maneuver_4) 
  
}