#### Base Functions as needed for calculating the average friction force on all tyres of a vehicle

# to learn about these roxygen tags see: https://roxygen2.r-lib.org/articles/rd.html

source("R/Tyrelabel_conversion.R")

## Vehicle parameters

#'@param A_vehicle Frontal area of vehicle in (m^2)
#'@param c_drag Aerodynamic drag coefficient of the vehicle (unitless)
#'@param m_rotate Mass of the rotating parts (kg)
#'@param m_vehicle Mass of the vehicle (kg)

## Tyre quality and design parameters
#'@param c_roll The roll coefficient of the tyre (kg/kg)
#'@param mu_max_tyre_track Peak friction coefficient between tyre and track (unitless)
#'@param grip_index_tyre Tyre grip index number from EU label
#'@param c_max_brake The maximum braking constant the tyres can achieve (m/s^2)
#'@param wet_mu_max_ref_tyre The friction coefficient of an EU reference tyre on wet asphalt (0.85)
#'@param optimal_slip_ratio_tyre_track The slip ratio between the tyre and the track at the peak friction coefficient

## Road surface underground parameters
#'@param apha_bank_slope Bank slope of the road in latitudinal direction (rad)
#'@param alpha_slope Slope of the road in longitudinal direction (rad)' 
#'@param optimal_ratio_slip_track Optimal slip ratio of the track underground
#'@param r_corner Radius of the corner (m)
#'@param x_correct_mu_max_track A correction factor for the maximum friction coefficient across different undergrounds
#'
## Landscape parameters
#'@param rho_air Density of air (kg/m^3)
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

#### Longitudinal friction force Functions

#'@section Aerodynamic drag force
#Aerodynamic drag force is defined as the force which is faced by the vehicle as it moves through the air.
#The aerodynamic drag force is present during acceleration, deceleration and constant speed maneuvers.
#It is calculated from the density of air (rho_air), the velocity of the wind (v_wind) and vehicle (v_vehicle) 
#as well as the vehicles drag coefficient (c_drag) and the vehicle frontal area (A_vehicle).

f_drag_force <- function(c_drag, A_vehicle, rho_air, v_vehicle, v_wind) {pmax(0,c_drag*A_vehicle*rho_air*(((v_vehicle)+v_wind)^2)/2)}

#'@section Roll resistance force
# Rolling resistance force (F_roll) is defined as the force that resists the motion the tyres rolling on the track surface.
# The rolling resistance force is present during acceleration, deceleration and constant speed maneuvers.
# It is calculated as the product of the roll resistance coefficient (c_roll) defined for the tyre-track interface, 
# the vehicles mass (m_vehicle) and the gravitational acceleration constant (grav_constant).

f_roll_force <- function(c_roll, m_vehicle, grav_constant) {c_roll*m_vehicle*grav_constant}

#'@section Slope force
# The slope force (also called grade force) refers to the in- or declination of the surface road to the horizontal in longitudinal direction.
# The slope force (F_slope) is calculated as the product of the vehicle mass (m_vehicle), the gravitational constant (grav_constant) 
# and the sinus of the slope angle of the road (alpha_slope) in degrees.
# As such, slope force is zero in case there is no road in- or declination (alpha_slope = 0).
# It is included as a longitudinal resistant longitudinal force in case the road inclines (alpha_slope > 0) and the vehicle thus drives 
# in uphill direction. The slope force is as such present as resistant longitudinal force during upon uphill driving acceleration, deceleration 
# and constant speed maneuvers. In case the road declines (alpha_slope < 0) the slope force is considered a propulsive force that 
# does not yield friction at the tyres. 
# However, at steep hills and low speed limits, the driver may need to use the brakes to decelerate or to keep a constant speed under the speed limit.
# In that case slope force is included in the calculation of the level of brake force that is needed for deceleration and constant speed maneuvers.

f_slope_force <- function(m_vehicle, grav_constant, alpha_slope) {
  m_vehicle * grav_constant * sin(alpha_slope)
}

#'@section Brake force
#'Brake force (F_brake) is the force acting on the tyres as the driver uses the brakes of the vehicle.
#'The calculation of the level of brake force needed strongly depends on the maneuver the performed.
#'In case of a deceleration maneuver the brake force needed is calculated from the deceleration constant of the performed maneuver (c_decel),
#'the other resistant forces that also slow the vehicle down, e.g. aerodynamic drag force, rolling resistance force and inclining slope force,
#'the mass of the vehicle and the mass of the rotating parts of the vehicle.
#'The minimum brake force is limited to zero.
#'The slope force is negative at downhill driving, so that more brake force is needed to compensate downhill slope force as a propulsive.

f_decel_brake_force <- function(c_decel, 
                                m_vehicle,
                                m_rotate,
                                A_vehicle,
                                c_roll, 
                                grav_constant, 
                                rho_air, 
                                v_start_decel, 
                                v_end_decel, 
                                v_wind, 
                                alpha_slope,
                                c_drag){
  pmax(0,(c_decel * (m_vehicle + m_rotate) 
          - f_roll_force(c_roll, m_vehicle, grav_constant)
          - f_drag_force(c_drag, A_vehicle, rho_air, v_vehicle = mean(v_start_decel,v_end_decel), v_wind)
          - f_slope_force(m_vehicle, grav_constant, alpha_slope)))
}

# Brake force can be needed in cases the vehicle is driving steeply downhill and the driver needs to remain under the speed limit.
# In that case the deceleration constant is zero (c_decel =0), so that the equation for the brake force needed can be simplified to:

f_const_speed_brake_force <- function (m_vehicle, 
                                       grav_constant, 
                                       alpha_slope, 
                                       c_roll, c_drag, A_vehicle, rho_air, v_vehicle, v_wind)
{pmax(0, -(f_roll_force(c_roll, m_vehicle, grav_constant)

           + f_drag_force(c_drag=c_drag, A_vehicle=A_vehicle, rho_air=rho_air, 
                          v_vehicle = v_vehicle, v_wind=v_wind)
           + f_slope_force(m_vehicle=m_vehicle, grav_constant=grav_constant, alpha_slope=alpha_slope)))}


# For constant speed driving, the minimum brake force is also limited to zero.       

#'@section Inertia force 
# Inertia force lets bodies remain in their state, either at rest or in motion.
# The direction of inertia opposes the change of the speed.
# As such it acts as a resistive force for an accelerating vehicle and as a tractive force for a decelerating vehicle.
# The vehicle thus needs to overcome an inertia force during acceleration, 
# which is calculated as the sum of the vehicle mass and rotating parts multiplied by the acceleration constant the driver performs.

f_accel_inert_force <- function(m_vehicle, m_rotate, c_accel) {(m_vehicle+m_rotate)*c_accel}

#'@section Total longitudinal resistant forces per maneuver (N)
# The friction work is calculated per maneuver performed,
# which are acceleration, deceleration and constant speed driving.

#'@section Total longitudinal resistant force upon acceleration (N)
# The longitudinal resistant forces considered during acceleration are the
# aerodynamic drag force, roll resistance force, uphill slope force, and acceleration inertia force.
#'The total resistant forces during acceleration are the inertia force plus roll, drag and uphill slope force. 
#'In case slope is negative, slope force becomes tractive instead of resistant 
#'and therefore the minimum slope force is not smaller than zero.

f_accel_long_force <- function(c_roll, m_vehicle, 
                               grav_constant, c_drag, A_vehicle, rho_air, 
                               v_start_accel, v_end_accel, v_wind, alpha_slope, 
                               m_rotate, c_accel){
  if(v_start_accel == v_end_accel | v_end_accel < v_start_accel) return(0)
  f_roll_force(c_roll=c_roll, 
               m_vehicle=m_vehicle, 
               grav_constant=grav_constant)
  + f_drag_force(c_drag=c_drag, 
                 A_vehicle=A_vehicle, 
                 rho_air=rho_air, 
                 v_vehicle=mean(v_start_accel, v_end_accel), 
                 v_wind=v_wind)
  + pmax(0,f_slope_force(m_vehicle=m_vehicle, 
                         grav_constant=grav_constant, 
                         alpha_slope=alpha_slope))
  + f_accel_inert_force(m_vehicle=m_vehicle, 
                        m_rotate=m_rotate, 
                        c_accel=c_accel)
}


#'@section Total longitudinal resistant force upon deceleration (N)
#'The longitudinal forces during deceleration are the aerodynamic drag force, the rolling resistance force,
#'the uphill slope force and if necessary an additional brake force the driver needs to slow down with a given deceleration constant.

f_decel_long_force <- function(m_vehicle, 
                               c_roll,
                               c_drag,
                               A_vehicle,
                               grav_constant, 
                               rho_air, 
                               v_start_decel, 
                               v_end_decel, 
                               v_wind, 
                               alpha_slope,
                               m_rotate,
                               c_decel){
  if(v_start_decel == v_end_decel | v_start_decel < v_end_decel) return(0)
  roll_force = f_roll_force(c_roll=c_roll, 
                            m_vehicle=m_vehicle, 
                            grav_constant=grav_constant)
  drag_force =  f_drag_force(c_drag=c_drag, 
                             A_vehicle=A_vehicle, 
                             rho_air=rho_air, 
                             v_vehicle = mean(v_start_decel,v_end_decel), 
                             v_wind=v_wind)
  
  slope_force = pmax(0, f_slope_force(m_vehicle=m_vehicle, 
                                      grav_constant=grav_constant, 
                                      alpha_slope=alpha_slope))
  
  decel_brake_force = f_decel_brake_force(c_decel=c_decel, 
                                          m_vehicle=m_vehicle, 
                                          A_vehicle=A_vehicle,
                                          m_rotate=m_rotate, 
                                          c_roll=c_roll, 
                                          grav_constant=grav_constant, 
                                          rho_air=rho_air, 
                                          v_start_decel=v_start_decel, 
                                          v_end_decel=v_end_decel, 
                                          v_wind=v_wind, 
                                          alpha_slope=alpha_slope,
                                          c_drag=c_drag)
  
  return(roll_force + drag_force + slope_force + decel_brake_force)
}

#'@section Total longitudinal resistant force at constant speed driving (N)
#'The longitudinal forces during constant speed driving are the aerodynamic drag force, the rolling resistance force,
#'the uphill slope force
#'or if necessary an additional brake force the driver needs to remain under a speed limit at steep downhill driving.
f_const_speed_long_force <- function(c_drag, 
                                     A_vehicle, 
                                     rho_air, 
                                     v_vehicle, 
                                     v_wind, 
                                     c_roll, 
                                     m_vehicle, 
                                     grav_constant, 
                                     alpha_slope){
  f_drag_force(c_drag, A_vehicle, rho_air, v_vehicle, v_wind) +
    f_roll_force(c_roll, m_vehicle, grav_constant) +
    pmax(0,f_slope_force(m_vehicle, grav_constant, alpha_slope)) +
    f_const_speed_brake_force(m_vehicle, grav_constant, alpha_slope, c_roll, c_drag, A_vehicle, rho_air, v_vehicle, v_wind)
}


#'@section Longitudinal slip functions
#' Longitudinal slip is defined here as the difference between the actual forward vehicle velocity and 
#' the radiant velocity of the wheels.The model includes two type of slip activities which are: 
#' (i) wheelspin: the wheel velocity is faster than the forward velocity of the vehicle but slips due to a lack of traction 
#' and (ii) braking: vehicle is still moving in forward direction but a brake force limits the wheel velocity.

#'@section Wheelspin slip
#'The peak friction coefficient (mu_max_tyre_track) is defined here as the measure of tyre to road surface friction based on the maximum deceleration of a rolling tyre.
#'As such, the peak friction coefficient can differ for each combination of tyre as well as the type, wettiness and temperature of the road surface. 
#'Measurement data on the peak friction coefficient are available for the specific circuit test runs performed at IDIADA.
#'In case, such measurement data is not available the peak friction coefficient for wet asphalt can be estimated 
#'from the tyres's wet grip index as indicated by the EU quality label 
#'and the peak friction coefficient of a reference tyre in EU testing protocols (wet_mu_max_ref_tyre) which has a value of 0.85.
#' For other undergrounds then wet asphalt the peak friction coefficient can be calculated using a correction factor.
#' @param Underground The type of underground, for instace dry asfalt, in order to correct for the data available by default for wet asfalt, such as the wet grip index.
f_mu_max_tyre_track <- function (grip_index_tyre, wet_mu_max_ref_tyre,x_correct_mu_max_track ){
  # for wet asfalt:
  wet_mu_max_tyre_track = (grip_index_tyre * wet_mu_max_ref_tyre) / 1.25
  # so for dry asfalt:
  wet_mu_max_tyre_track * x_correct_mu_max_track
}


### brake deceleration of candidate tyre correction from wet grip testing to specific road track m.s^-2
f_c_max_brake <-
  function (grip_index_tyre, # Grip index, derived from tyre label
            c_brake_ref_tyre_wet, # ref testing break decelaration (0.68 g)
            x_correct_road, # correction for calculating dry situation from wet test
            grav_constant) {
    (grip_index_tyre * c_brake_ref_tyre_wet * grav_constant) / 1.25 * x_correct_road
  }

#'The optimal slip ratio refers to level of slip performed on the tyre during a maneuver performed with a friction coefficient (mu_tyre_track)
#'that is equal or larger than the peak friction coefficient, such as full acceleration or steep downhill driving on slippery surface as ice.
#'The maneuvers simulated however are in the so called low slip regime (mu_tyre_track < mu_max_tyre_track),
#'where wheelspin slip increases with the longitudinal forces performed on the tyres. 
#'The friction coefficient during a maneuver can be calculated as the sum of the longitudinal forces acting on the tyres
#'divided by the downward normal load force perpendicular to the longitudinal direction.

f_long_normal_load_force <- function(alpha_slope,m_vehicle, grav_constant)
{cos(alpha_slope)*(m_vehicle*grav_constant)}

#' As such, the friction coefficient upon acceleration is calculated as 
#' the total longitudinal acceleration force divided by the normal longitudinal load force: 

f_accel_long_mu_slip <- function(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel)
{f_accel_long_force(c_roll, m_vehicle, grav_constant, c_drag, 
                    A_vehicle, rho_air, v_start_accel, v_end_accel,
                    v_wind, alpha_slope, m_rotate, c_accel)*
    1/(f_long_normal_load_force(alpha_slope, m_vehicle, grav_constant))}

#' The wheelspin slip during acceleration can then calculated as the ratio of the friction coefficient to the peak friction coefficient multiplied by the optimal slip ratio.

f_accel_wheelspin_slip <- function (c_roll, m_vehicle, grav_constant, c_drag, 
                                    A_vehicle, rho_air, mu_max_tyre_track,
                                    v_start_accel, v_end_accel, v_wind, alpha_slope, 
                                    m_rotate, c_accel, optimal_slip_ratio_tyre_track)
{f_accel_long_mu_slip (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, 
                       v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, c_accel)*
    (1/mu_max_tyre_track)*optimal_slip_ratio_tyre_track} 


#' The friction coefficient at constant speed driving is calculated as
#' the total longitudinal force divided by the normal longitudinal load force.

# f_constant_speed_long_mu_slip <- function(c_roll, m_vehicle, grav_constant, 
#                                           c_drag, A_vehicle, rho_air, v_vehicle, 
#                                           v_wind, alpha_slope)
# {f_const_speed_long_force(c_drag, A_vehicle, rho_air, v_vehicle, v_wind, 
#                           c_roll, m_vehicle, grav_constant, alpha_slope)*
#     1/(f_long_normal_load_force(alpha_slope, m_vehicle, grav_constant))}

#' The wheelspin slip at constant driving is then calculated as the ratio of the friction coefficient 
#' to the peak friction coefficient multiplied by the optimal slip ratio.

# f_constant_speed_wheelspin_slip <- function(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope, mu_max_tyre_track, optimal_slip_ratio_tyre_track)
# {f_constant_speed_long_mu_slip(c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope)*
#     (1/mu_max_tyre_track)*optimal_slip_ratio_tyre_track}

#' The friction coefficient at deceleration is calculated as
#' the total longitudinal force divided by the normal longitudinal load force.
# f_decel_long_mu_slip <- function(m_vehicle, 
#                                  c_roll, 
#                                  grav_constant, 
#                                  rho_air, 
#                                  v_start_decel, 
#                                  v_end_decel, 
#                                  v_wind, 
#                                  alpha_slope, 
#                                  c_decel,
#                                  c_drag,
#                                  m_rotate){
#   
#   f_decel_long_force(m_vehicle=m_vehicle, 
#                      c_roll=c_roll, 
#                      grav_constant=grav_constant, 
#                      rho_air=rho_air, 
#                      v_start_decel=v_start_decel, 
#                      v_end_decel=v_end_decel, 
#                      v_wind=v_wind, 
#                      alpha_slope=alpha_slope, 
#                      c_decel=c_decel,
#                      m_rotate=m_rotate,
#                      c_drag=c_drag)*
#     (1/f_long_normal_load_force(alpha_slope=alpha_slope,
#                                 m_vehicle=m_vehicle,
#                                 grav_constant=grav_constant))}


#' The wheelspin slip at deceleration is then calculated as the ratio of the friction coefficient 
#' to the peak friction coefficient multiplied by the optimal slip ratio.

# f_decel_wheelspin_slip <- function(m_vehicle, 
#                                    c_roll, 
#                                    grav_constant, 
#                                    rho_air, 
#                                    v_start_decel, 
#                                    v_end_decel, 
#                                    v_wind, 
#                                    alpha_slope, 
#                                    c_decel, 
#                                    mu_max_tyre_track, 
#                                    optimal_slip_ratio_tyre_track){
#   
#   
#   decel_long_mu_slip = f_decel_long_force(m_vehicle=m_vehicle, 
#                                             c_roll=c_roll, 
#                                             grav_constant=grav_constant, 
#                                             rho_air=rho_air, 
#                                             v_start_decel=v_start_decel, 
#                                             v_end_decel=v_end_decel, 
#                                             v_wind=v_wind, 
#                                             alpha_slope=alpha_slope, 
#                                             c_decel=c_decel,
#                                             m_rotate=m_rotate,
#                                             c_drag=c_drag) *
#     (1/f_long_normal_load_force(alpha_slope=alpha_slope,
#                                 m_vehicle=m_vehicle,
#                                 grav_constant=grav_constant))
#   
#   decel_long_mu_slip * (1/mu_max_tyre_track) * optimal_slip_ratio_tyre_track
# }

#'@section Brake slip
#'During a brake maneuver a brake force causes the wheels to stop rotating.
#'The slip ratio is 100% during a full wheellock brake maneuver 
#'that is performed with a maximum brake force the tyres can endure.
#'The slip of a braking maneuver is calculated as the ratio of the brake force performed during the maneuver 
#'to the maximum brake force the tyres can endure.

# f_decel_brake_slip <-function (c_decel, m_vehicle, m_rotate, c_roll, grav_constant, 
#                                rho_air, v_start_decel, v_end_decel, 
#                                v_wind, alpha_slope, c_max_brake,
#                                c_drag, A_vehicle){ 
#   
#   max_brake_force = (m_vehicle+m_rotate)*c_max_brake
#   
#   decel_brake_force = f_decel_brake_force(c_decel=c_decel, 
#                                           m_vehicle=m_vehicle, 
#                                           A_vehicle=A_vehicle,
#                                           m_rotate=m_rotate, 
#                                           c_roll=c_roll, 
#                                           grav_constant=grav_constant, 
#                                           rho_air=rho_air, 
#                                           v_start_decel=v_start_decel, 
#                                           v_end_decel=v_end_decel, 
#                                           v_wind=v_wind, 
#                                           alpha_slope=alpha_slope,
#                                           c_drag=c_drag)
#   
#   return(decel_brake_force / max_brake_force)
# }

#' The total longitudinal slip during a deceleration maneuver is calculated as:
f_decel_long_slip <- function (c_decel, m_vehicle, m_rotate, c_roll, grav_constant, 
                               rho_air, v_start_decel, v_end_decel, v_wind, 
                               alpha_slope, optimal_slip_ratio_tyre_track,
                               grip_index_tyre, 
                               wet_mu_max_ref_tyre, c_brake_ref_tyre_wet,
                               x_correct_mu_max_track,
                               c_drag, A_vehicle){
  
  c_max_brake = f_c_max_brake(grip_index_tyre=grip_index_tyre, # Grip index, derived from tyre label
                              c_brake_ref_tyre_wet, # ref testing break decelaration (0.68 g)
                              x_correct_road=x_correct_mu_max_track, # correction for calculating dry situation from wet test
                              grav_constant=grav_constant)
  
  mu_max_tyre_track = f_mu_max_tyre_track(grip_index_tyre=grip_index_tyre, 
                                          wet_mu_max_ref_tyre=wet_mu_max_ref_tyre, 
                                          x_correct_mu_max_track=x_correct_mu_max_track)
  
  #' The friction coefficient at deceleration (decel_long_mu_slip) is calculated as
  #' the total longitudinal force divided by the normal longitudinal load force.
  decel_long_force = f_decel_long_force(m_vehicle=m_vehicle, 
                                        c_roll=c_roll, 
                                        grav_constant=grav_constant, 
                                        rho_air=rho_air, 
                                        v_start_decel=v_start_decel, 
                                        v_end_decel=v_end_decel, 
                                        v_wind=v_wind, 
                                        alpha_slope=alpha_slope, 
                                        c_decel=c_decel,
                                        m_rotate=m_rotate,
                                        c_drag=c_drag,
                                        A_vehicle = A_vehicle)
  long_normal_load_force = f_long_normal_load_force(alpha_slope=alpha_slope,
                                                    m_vehicle=m_vehicle,
                                                    grav_constant=grav_constant)
  
  decel_long_mu_slip =  decel_long_force / long_normal_load_force
  
  #' The wheelspin slip at deceleration (decel_wheelspin_slip) is then calculated as the ratio of the friction coefficient 
  #' to the peak friction coefficient multiplied by the optimal slip ratio.
  
  decel_wheelspin_slip =  (decel_long_mu_slip / mu_max_tyre_track) * optimal_slip_ratio_tyre_track
  
  # The maximum brake force (max_brake_force) is calculated as 
  #' the maximum braking deceleration constant (c_max_brake) achieved in a the tyre test at the track
  #' multiplied with the sum of the vehicle mass and the mass of the rotating parts.
  max_brake_force = (m_vehicle+m_rotate)*c_max_brake
  
  decel_brake_force = f_decel_brake_force(c_decel=c_decel, 
                                          m_vehicle=m_vehicle, 
                                          A_vehicle=A_vehicle,
                                          m_rotate=m_rotate, 
                                          c_roll=c_roll, 
                                          grav_constant=grav_constant, 
                                          rho_air=rho_air, 
                                          v_start_decel=v_start_decel, 
                                          v_end_decel=v_end_decel, 
                                          v_wind=v_wind, 
                                          alpha_slope=alpha_slope,
                                          c_drag=c_drag)
  
  #' Brake slip during deceleration is then calculated as 
  #' the brake force needed to decelerate divided by the maximum brake force. 
  decel_brake_slip = decel_brake_force / max_brake_force
  
  decel_wheelspin_slip * (1-decel_brake_slip) + decel_brake_slip
}




#'@section Summing wheelspin and brake slip per maneuver
#'
#' The total longitudinal slip during an acceleration maneuver is calculated as wheelspin only:
f_accel_long_slip <- function (c_roll, m_vehicle, grav_constant, c_drag, A_vehicle, rho_air, 
                               v_start_accel, v_end_accel, v_wind, alpha_slope, m_rotate, 
                               c_accel, optimal_slip_ratio_tyre_track, grip_index_tyre, wet_mu_max_ref_tyre,
                               x_correct_mu_max_track){
  
  mu_max_tyre_track = f_mu_max_tyre_track(grip_index_tyre=grip_index_tyre, 
                                          wet_mu_max_ref_tyre=wet_mu_max_ref_tyre, 
                                          x_correct_mu_max_track=x_correct_mu_max_track)
  f_accel_wheelspin_slip(c_roll=c_roll, 
                         m_vehicle=m_vehicle, 
                         grav_constant=grav_constant, c_drag=c_drag, A_vehicle=A_vehicle, 
                         mu_max_tyre_track=mu_max_tyre_track, rho_air=rho_air, 
                         v_start_accel=v_start_accel, v_end_accel=v_end_accel, 
                         v_wind=v_wind, alpha_slope=alpha_slope, m_rotate=m_rotate, 
                         c_accel=c_accel, optimal_slip_ratio_tyre_track=optimal_slip_ratio_tyre_track)
  
}



#' The total longitudinal slip during a constant speed maneuver is calculated as:
f_const_speed_long_slip <- function (c_roll, m_vehicle, m_rotate, grav_constant, c_drag, 
                                     A_vehicle, rho_air, v_vehicle, v_wind, alpha_slope,
                                     wet_mu_max_ref_tyre, optimal_slip_ratio_tyre_track,
                                     grip_index_tyre, c_brake_ref_tyre_wet, x_correct_mu_max_track){
  
  const_speed_long_force = f_const_speed_long_force(c_drag, A_vehicle, rho_air, v_vehicle, v_wind, 
                                                    c_roll, m_vehicle, grav_constant, alpha_slope)
  long_normal_load_force = f_long_normal_load_force(alpha_slope, m_vehicle, grav_constant)
  
  #' The friction coefficient at constant speed driving is calculated as
  #' the total longitudinal force divided by the normal longitudinal load force.
  constant_speed_long_mu_slip = const_speed_long_force / long_normal_load_force
  
  mu_max_tyre_track = f_mu_max_tyre_track(grip_index_tyre=grip_index_tyre, 
                                          wet_mu_max_ref_tyre=wet_mu_max_ref_tyre, 
                                          x_correct_mu_max_track=x_correct_mu_max_track)
  #' The wheelspin slip at constant driving is then calculated as the ratio of the friction coefficient 
  #' to the peak friction coefficient multiplied by the optimal slip ratio.
  constant_speed_wheelspin_slip = (constant_speed_long_mu_slip / mu_max_tyre_track) * optimal_slip_ratio_tyre_track
  
  
  c_max_brake = f_c_max_brake(grip_index_tyre=grip_index_tyre, # Grip index, derived from tyre label
                              c_brake_ref_tyre_wet=c_brake_ref_tyre_wet, # ref testing break decelaration (0.68 g)
                              x_correct_road=x_correct_mu_max_track, # correction for calculating dry situation from wet test
                              grav_constant=grav_constant)
  
  #' Brake slip at constant speed driving is calculated as 
  #' the brake force needed to remain under the speed limit divided by the maximum brake force.
  max_brake_force = (m_vehicle+m_rotate)*c_max_brake
  
  const_speed_brake_force = f_const_speed_brake_force(m_vehicle=m_vehicle, 
                                                      grav_constant=grav_constant, 
                                                      alpha_slope=alpha_slope, 
                                                      c_roll=c_roll, 
                                                      c_drag=c_drag,
                                                      A_vehicle=A_vehicle,
                                                      rho_air=rho_air, 
                                                      v_vehicle=v_vehicle, 
                                                      v_wind=v_wind)
  const_speed_brake_slip = const_speed_brake_force / max_brake_force
  
  constant_speed_wheelspin_slip * (1-const_speed_brake_slip) + const_speed_brake_slip
}


#'@section Latitudinal friction force functions

#'@section Resultant latitudinal force
#'Then bank slope force works in opposite direction to the centripetal force, 
#'so that the resultant lateral forces on the tyres is the centripetal force minus the bank force.
#'In case the bank slope is negative, centripetal and bank force work in the same direction. 
#'In case the bank slope force is greater than the centripetal force, 
#'the resultant latitudinal force equals the bank force minus the centripetal force.
#'The resultant latitudinal force is not to be negative. 
#'Therefore, the rules above are included in the routine as ((centripet_force - bank_force)^2)^0.5

f_lat_force <- function(m_vehicle , v_vehicle , r_corner, grav_constant, alpha_bank_slope){
  
  #'Centripetal force at corners
  if(r_corner == 0 | is.na(r_corner)) { centripet_force = 0 } else {
    centripet_force = (m_vehicle*(v_vehicle^2)/r_corner)*cos(alpha_bank_slope)
  }
  #'Bank slope force slope is positive if the high side of the bank is on the outside of the corner. 
  
  bank_force = grav_constant*sin(alpha_bank_slope)*m_vehicle
  
  return(abs(centripet_force - bank_force))
} 

#' @section Latitudinal slip

#' The normal load force is calculated as the cosinus of the bank slope degree multiplied with vehicle weight.
f_lat_normal_load_force <- function(alpha_bank_slope,m_vehicle, grav_constant, r_corner, v_vehicle){
  #'Centripetal force at corners
  if(r_corner == 0 | is.na(r_corner)) { n_centripet_force = 0 } else {
    n_centripet_force = (m_vehicle*(v_vehicle^2)/r_corner)*sin(alpha_bank_slope)
  }
  n_bank_force = cos(alpha_bank_slope)*(m_vehicle*grav_constant)
  
  return(abs(n_centripet_force) + abs(n_bank_force))
}


#' The latitudinal slip is then calculated as the latitudinal friction coefficient divided by the peak friction coefficient multiplied with the optimal slip ratio.

f_lat_slip <- function (m_vehicle , v_vehicle , r_corner, grav_constant, 
                        alpha_bank_slope, optimal_slip_ratio_tyre_track,
                        grip_index_tyre, wet_mu_max_ref_tyre, x_correct_mu_max_track){
  #' The normal load force is calculated as the cosinus of the bank slope degree multiplied with vehicle weight.
  #' 
  # browser()
  lat_normal_load_force = f_lat_normal_load_force(alpha_bank_slope=alpha_bank_slope,
                                                  m_vehicle=m_vehicle, 
                                                  grav_constant=grav_constant, 
                                                  r_corner=r_corner, 
                                                  v_vehicle=v_vehicle)
  lat_force = f_lat_force(m_vehicle=m_vehicle , 
                          v_vehicle=v_vehicle , 
                          r_corner=r_corner, 
                          grav_constant=grav_constant, 
                          alpha_bank_slope=alpha_bank_slope)
  #'The latitudinal friction coefficient is calculated by dividing the resultant latitudinal forces 
  #' with the normal load force perpendicular to the latitudinal forces
  lat_mu_slip = lat_force/lat_normal_load_force
  
  mu_max_tyre_track = f_mu_max_tyre_track(grip_index_tyre=grip_index_tyre, 
                                          wet_mu_max_ref_tyre=wet_mu_max_ref_tyre, 
                                          x_correct_mu_max_track=x_correct_mu_max_track)
  
  (lat_mu_slip/mu_max_tyre_track)*optimal_slip_ratio_tyre_track
}

#' @section Distance and time functions ####
#' Distance and time functions are used to calculate the distances and times over which driving maneuvers 
#' take place. The functions include f_accel_time, f_accel_distance, f_decel_time and 
#' f_decel_distance and f_corner_distance. 
#' The acceleration time is calculated from the starting velocity (m/s), end velocity (m/s) and accelaration constant (m/s^2) of the maneuver

f_accel_distance <- function(v_start , v_end , c_accel ) {
  #' The acceleration time is calculated from the starting velocity (m/s), end velocity (m/s) and accelaration constant (m/s^2) of the maneuver
  #' Acceleration time in s: 
  #' 
  if(c_accel == 0 & v_start == v_end)  return(0)
  if(c_accel == 0 & v_start < v_end)  stop("acceleration c_accel needed")  
  if(v_start > v_end) {
  return(0)
    }
  
  accel_time = (v_end-v_start)/c_accel
  
  v_start*accel_time + 1/2*c_accel*(accel_time^2)
}

#' Deceleration distance in m:

f_decel_distance <- function(v_start, v_end, c_decel ){
  #'Deceleration time in s:
  #'The deceleration time is calculated from the starting velocity (m/s), end velocity (m/s) and deceleration constant (m/s^2) of the maneuver
  if(c_decel == 0 & v_start == v_end) return(0)
  if(c_decel == 0 & v_start > v_end) stop("deceleration rate c_decel needed")
  if(v_start < v_end) { return(0) } # no distance for deceleration when acceleration is taking place
  
  decel_time = (v_start-v_end)/c_decel
  
  return(v_start*decel_time+1/2*c_decel*(decel_time^2))
  
}

#'@section Corner distance in m:
f_corner_distance <- function(r_corner,corner_angle){
  (corner_angle/(360)*2*r_corner*pi)
}
