Tyre wear model by Joris Meesters and Joris Quik

## Primary algorithms

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

## Maneuver Functions

### acceleration dry

    ## Simulating longitudinal friction work per track sector under dry conditions
    ### dry condition slip parameters

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

### deceleration dry

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

### constant speed dry

    f_const_dry <- function(sector_velocity_ms,
                            sector_end_velocity_ms,
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
                            distance_maneuver_3 = pmax(0,sector_distance - distance_maneuver_1 - distance_maneuver_2)){
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

### cornering dry

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
           friction_work_longitude=longitude_friction_work_maneuver_4,
           friction_work_latitude = latitude_friction_work_maneuver_4) 
      
    }

## Sensitivity analysis

### Data inputs

    #### Overview of default data values #####
    n_runs <- 10000
    # input variables as used in calculations/sensitivity analysis

    input_data <- tibble(
      #Gravitational constant in m.s^-2
      'grav_constant'=9.81,
      ##### Landscape data #####
      #Density of air in kg.m^-3
      'rho_air'=1.205,
      # Wind speed in m.s^-1
      'v_wind' = runif(n=n_runs ,-5,5),
      
      # Landscape slope in %
      'grade_slope' = runif(n=n_runs,-10,10)/100,
      'grade_bank_slope' = runif(n=n_runs,-10,10)/100,
      
      ##### Track-tire default data values #####
      #Roll resistance coefficient in kg/kg at dry asphalt
      'c_roll'= runif(n=n_runs,1,10)/1000,
      #Grip index tyre
      'grip_index_tyre' = runif(n=n_runs,1.09,1.55),
      # Peak friction coefficient of reference tyre in reference grip testing
      'mu_max_ref_tyre' = 0.85,
      # correction mu max for road underground
      'x_correct_road_wet_asphalt' = 1,
      'x_correct_road_dry_asphalt' =runif(n=n_runs,1.07,1.47),
      # optimal slip ratios per underground
      'optimal_slip_dry_asphalt' = runif(n=n_runs,0.15,0.20),
      'optimal_slip_wet_asphalt' = runif(n=n_runs,0.08,0.12),
      
      ##### Vehicle data #####
      #Vehicle acceleration from 0-100 km.h^-1 in s
      't_0_100kmh' =runif(n=n_runs,8,20),
      #Vehicle mass in kg
      'm_vehicle'=runif(n=n_runs,900,3000),
      #Vehicle surface area in m^2
      'A_vehicle'=runif(n=n_runs,2,4),
      #Vehicle aerodynamic drag coefficient
      'c_drag'=runif(n=n_runs,0.02,0.5),
      #vehicle turning diameter
      'd_turn_vehicle' = 9.8,
      
      ##### Driving style data #####
      # the percentage of the maximum acceleration constant at which the driver feels comfortable 
      # for uniform distribution
      'p_min_acc' = 10, # minimum % of the maximum acceleration constant at which the driver feels comfortable
      'p_max_acc' = 100, # maximum % of the maximum acceleration constant at which the driver feels comfortable
      
      #Comfortable deceleration constant
      'c_decel_comfort' = runif(n=n_runs,2,3),
      
      ##### Road data #####
      #Corner radius in m
      'r_corner' = runif(n=n_runs,10,50),
      # corner angle in degrees
      'angle_corner' = runif(n=n_runs,0,360)
    )

    # RSTUDIOn shortcut for forward pipe operator: Ctrl + Shift + M
    # https://shiny.rivm.nl/kasstvdj/R_cursussen/Tidy-R_cursus/introductie-tidyverse.html#forward-pipe-operator

    input_data <- input_data |> 
      mutate(
        # Landscape slope in Degrees
        'alpha_slope' = atan(grade_slope)*(180/pi),
        'alpha_bank_slope' = atan(grade_bank_slope)*(180/pi),
        
        #Brake deceleration constant of reference tyre in reference wet grip testing in m.s^-2
        'c_full_brake_ref_tyre' = 0.68*grav_constant,
        
        #Maximum acceleration constant in m.s^-2
        'c_accel_max' = (100*(1000/3600))/t_0_100kmh,
        
        # Total mass of rotating vehicle parts in kg
        'm_rotate'= (runif(n=n_runs, 13,15)/100)*m_vehicle,
        
        'frac_driver_comfort_max_accelaration' = runif(n=n_runs,p_min_acc,p_max_acc)/100
        
      )

Currently the functions depend on track data, which is prepared here.

    #### Friction forces per maneuver ####
    ##### Constant speed straight driving #####

    # Scenario for urban traffic
    kph_min <- 0 # kmph
    kph_max <- 70 # kmph
    # what is representative minimum radius for highway corner?
    # What is representative corner angle? (max)
    # what is representative slope min/max?

    # Scenario for highway traffic
    # kph_min <- 60 # kmph
    # kph_max <- 140 # kmph
    # what is representative minimum radius for highway corner?
    # What is representative corner angle? (max)
    # what is representative slope min/max?

    # Scenario for stop and go traffic
    # kph_min <- 60 # kmph
    # kph_max <- 140 # kmph
    # what is representative minimum radius for highway corner?
    # What is representative corner angle? (max)
    # what is representative slope min/max?

    # Scenario for rural traffic
    # kph_min <- 0 # kmph
    # kph_max <- 90 # kmph
    # what is representative minimum radius for highway corner?
    # What is representative corner angle? (max)
    # what is representative slope min/max?


    input_data <- input_data |> mutate(
      #Track sector simulations
      ## Track sector data
      'sector_distance' = 1000,
      'sector_velocity_kmh' = runif(n_runs,kph_min,kph_max),
      'sector_velocity_ms' = sector_velocity_kmh*1000/3600,
      
      # Is it not better to use sector_velocity as the starting point always? and the sector end velocity always as the end velocity which can result in acc or decc in the sector for x distance of this meneuver?
      
      'sector_start_velocity_kmh' = runif(n_runs,kph_min,kph_max),
      'sector_start_velocity_ms' = sector_start_velocity_kmh*1000/3600,
      'sector_end_velocity_kmh' = runif(n_runs,kph_min,kph_max),
      'sector_end_velocity_ms' = sector_end_velocity_kmh*1000/3600,
      'sector_underground' = "dry asphalt",
      'sector_alpha_slope' = alpha_slope,
      # 'sector_latitude' = 0 ,
      # 'sector_corner_radius' = 10,
      'sector_bank_slope' = alpha_bank_slope,
      # 'sector_corner_angle' = 180
    )

    ## Vehicle and tyre performances per track underground
    input_data <- input_data |> 
      mutate(
        ### brake deceleration constant on dry asphalt
        'c_full_brake_dry_asphalt' = 
          f_c_full_brake(grip_index_tyre = grip_index_tyre, 
                         x_correct_road = x_correct_road_dry_asphalt, 
                         c_full_brake_ref_tyre = c_full_brake_ref_tyre),
        ### brake deceleration constant on wet asphalt
        'c_full_brake_wet_asphalt' = 
          f_c_full_brake(grip_index_tyre = grip_index_tyre, 
                         x_correct_road = x_correct_road_wet_asphalt, 
                         c_full_brake_ref_tyre = c_full_brake_ref_tyre),
        ### linear increase before reaching optimal slip on dry asphalt
        'x_slip_long_force_dry_asphalt' = 
          f_x_slip_long_force(mu_max = 
                                ###peak friction coefficient on dry asphalt
                                f_mu_max(grip_index_tyre = grip_index_tyre,
                                         x_correct_road = x_correct_road_dry_asphalt, 
                                         mu_max_ref_tyre = mu_max_ref_tyre), 
                              ### Normal load force f 
                              normal_load_force = 
                                f_normal_load_force(m_vehicle = m_vehicle, 
                                                    grav_constant = grav_constant), 
                              optimal_slip = optimal_slip_dry_asphalt),
        ### linear increase before reaching optimal slip on wet asphalt
        'x_slip_long_force_wet_asphalt' = 
          f_x_slip_long_force(mu_max =  
                                ###peak friction coefficient on wet asphalt  
                                f_mu_max(grip_index_tyre = grip_index_tyre,
                                         x_correct_road = x_correct_road_wet_asphalt, 
                                         mu_max_ref_tyre = mu_max_ref_tyre), 
                              ### Normal load force f 
                              normal_load_force = 
                                f_normal_load_force(m_vehicle = m_vehicle, 
                                                    grav_constant = grav_constant), 
                              optimal_slip = optimal_slip_wet_asphalt)
      )

### calculate output

    ProbY <- input_data |>
      mutate(
        'firc_work_acc_dry' = f_acc_dry(sector_velocity_ms=sector_velocity_ms,
                                        sector_start_velocity_ms=sector_start_velocity_ms,
                                        frac_driver_comfort_max_accelaration=frac_driver_comfort_max_accelaration,
                                        c_accel_max=c_accel_max,
                                        m_vehicle=m_vehicle,
                                        m_rotate=m_rotate,
                                        c_drag=c_drag,
                                        A_vehicle=A_vehicle,
                                        rho_air=rho_air,
                                        v_wind=v_wind,
                                        c_roll=c_roll,
                                        grav_constant=grav_constant,
                                        sector_alpha_slope=sector_alpha_slope,
                                        optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                        x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt,
                                        accel_force_end_maneuver_1 = 0
        )$friction_work_longitude,
        'distance_acc_dry' = f_acc_dry(sector_velocity_ms=sector_velocity_ms,
                                       sector_start_velocity_ms=sector_start_velocity_ms,
                                       frac_driver_comfort_max_accelaration=frac_driver_comfort_max_accelaration,
                                       c_accel_max=c_accel_max,
                                       m_vehicle=m_vehicle,
                                       m_rotate=m_rotate,
                                       c_drag=c_drag,
                                       A_vehicle=A_vehicle,
                                       rho_air=rho_air,
                                       v_wind=v_wind,
                                       c_roll=c_roll,
                                       grav_constant=grav_constant,
                                       sector_alpha_slope=sector_alpha_slope,
                                       optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                       x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt,
                                       accel_force_end_maneuver_1 = 0
        )$distance,
        'firc_work_dec_dry' = f_dec_dry (sector_velocity_ms=sector_velocity_ms,
                                         sector_end_velocity_ms=sector_end_velocity_ms,
                                         c_decel_maneuver_2 = c_decel_comfort,
                                         m_vehicle=m_vehicle,
                                         m_rotate=m_rotate,
                                         c_drag=c_drag,
                                         A_vehicle=A_vehicle,
                                         rho_air=rho_air,
                                         v_wind=v_wind,
                                         c_roll=c_roll,
                                         grav_constant=grav_constant,
                                         c_full_brake_dry_asphalt=c_full_brake_dry_asphalt,
                                         sector_alpha_slope=sector_alpha_slope,
                                         optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                         x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt)$friction_work_longitude,
        'distance_dec_dry' = f_dec_dry (sector_velocity_ms=sector_velocity_ms,
                                        sector_end_velocity_ms=sector_end_velocity_ms,
                                        c_decel_maneuver_2 = c_decel_comfort,
                                        m_vehicle=m_vehicle,
                                        m_rotate=m_rotate,
                                        c_drag=c_drag,
                                        A_vehicle=A_vehicle,
                                        rho_air=rho_air,
                                        v_wind=v_wind,
                                        c_roll=c_roll,
                                        grav_constant=grav_constant,
                                        c_full_brake_dry_asphalt=c_full_brake_dry_asphalt,
                                        sector_alpha_slope=sector_alpha_slope,
                                        optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                        x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt)$distance,
        'firc_work_const_dry' = f_const_dry (sector_velocity_ms=sector_velocity_ms,
                                             sector_end_velocity_ms=sector_end_velocity_ms,
                                             m_vehicle=m_vehicle,
                                             m_rotate=m_rotate,
                                             c_drag=c_drag,
                                             A_vehicle=A_vehicle,
                                             rho_air=rho_air,
                                             v_wind=v_wind,
                                             c_roll=c_roll,
                                             grav_constant=grav_constant,
                                             sector_alpha_slope=sector_alpha_slope,
                                             c_full_brake_dry_asphalt=c_full_brake_dry_asphalt,
                                             optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                             x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt,
                                             distance_maneuver_3 = pmax(0,(sector_distance - 
                                                                             distance_acc_dry - 
                                                                             distance_dec_dry)))$friction_work_longitude,
        'distance_const_dry' = f_const_dry (sector_velocity_ms=sector_velocity_ms,
                                            sector_end_velocity_ms=sector_end_velocity_ms,
                                            m_vehicle=m_vehicle,
                                            m_rotate=m_rotate,
                                            c_drag=c_drag,
                                            A_vehicle=A_vehicle,
                                            rho_air=rho_air,
                                            v_wind=v_wind,
                                            c_roll=c_roll,
                                            grav_constant=grav_constant,
                                            sector_alpha_slope=sector_alpha_slope,
                                            c_full_brake_dry_asphalt=c_full_brake_dry_asphalt,
                                            optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                            x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt,
                                            distance_maneuver_3 = pmax(0,(sector_distance - 
                                                                            distance_acc_dry - 
                                                                            distance_dec_dry)))$distance,
        'firc_work_long_corn_dry' = f_corn_dry (sector_velocity_ms=sector_velocity_ms,
                                                sector_corner_radius=r_corner,
                                                m_vehicle=m_vehicle,
                                                m_rotate=m_rotate,
                                                sector_bank_slope=sector_bank_slope,
                                                sector_corner_angle=angle_corner,
                                                c_drag=c_drag,
                                                A_vehicle=A_vehicle,
                                                rho_air=rho_air,
                                                v_wind=v_wind,
                                                c_roll=c_roll,
                                                sector_alpha_slope=sector_alpha_slope,
                                                grav_constant=grav_constant,
                                                c_full_brake_dry_asphalt=c_full_brake_dry_asphalt,
                                                optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                                x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt)$friction_work_longitude,
        'firc_work_lat_corn_dry' = f_corn_dry (sector_velocity_ms=sector_velocity_ms,
                                               sector_corner_radius=r_corner,
                                               m_vehicle=m_vehicle,
                                               m_rotate=m_rotate,
                                               sector_bank_slope=sector_bank_slope,
                                               sector_corner_angle=angle_corner,
                                               c_drag=c_drag,
                                               A_vehicle=A_vehicle,
                                               rho_air=rho_air,
                                               v_wind=v_wind,
                                               c_roll=c_roll,
                                               sector_alpha_slope=sector_alpha_slope,
                                               grav_constant=grav_constant,
                                               c_full_brake_dry_asphalt=c_full_brake_dry_asphalt,
                                               optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                               x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt)$friction_work_latitude,
        'distance_corn_dry' = f_corn_dry (sector_velocity_ms=sector_velocity_ms,
                                          sector_corner_radius=r_corner,
                                          m_vehicle=m_vehicle,
                                          m_rotate=m_rotate,
                                          sector_bank_slope=sector_bank_slope,
                                          sector_corner_angle=angle_corner,
                                          c_drag=c_drag,
                                          A_vehicle=A_vehicle,
                                          rho_air=rho_air,
                                          v_wind=v_wind,
                                          c_roll=c_roll,
                                          sector_alpha_slope=sector_alpha_slope,
                                          grav_constant=grav_constant,
                                          c_full_brake_dry_asphalt=c_full_brake_dry_asphalt,
                                          optimal_slip_dry_asphalt=optimal_slip_dry_asphalt,
                                          x_slip_long_force_dry_asphalt=x_slip_long_force_dry_asphalt)$distance,
        
        .keep = "none"
      )

    ProbY |> summary()

    ##  firc_work_acc_dry distance_acc_dry  firc_work_dec_dry distance_dec_dry
    ##  Min.   :      0   Min.   :   0.00   Min.   :      0   Min.   :  0.00  
    ##  1st Qu.:      0   1st Qu.:   0.00   1st Qu.:      0   1st Qu.:  0.00  
    ##  Median :      0   Median :   0.00   Median :      0   Median :  0.00  
    ##  Mean   :  37664   Mean   :  40.63   Mean   : 212210   Mean   : 25.58  
    ##  3rd Qu.:   2404   3rd Qu.:  48.97   3rd Qu.:  19761   3rd Qu.: 36.37  
    ##  Max.   :4082843   Max.   :1060.20   Max.   :8225011   Max.   :255.81  
    ##  firc_work_const_dry distance_const_dry firc_work_long_corn_dry
    ##  Min.   :       0    Min.   :   0.0     Min.   :      0        
    ##  1st Qu.:   12657    1st Qu.: 906.4     1st Qu.:    880        
    ##  Median :   43456    Median : 981.9     Median :   5229        
    ##  Mean   : 3801652    Mean   : 933.8     Mean   : 390882        
    ##  3rd Qu.: 6831960    3rd Qu.:1000.0     3rd Qu.: 445491        
    ##  Max.   :24366464    Max.   :1000.0     Max.   :6150490        
    ##  firc_work_lat_corn_dry distance_corn_dry  
    ##  Min.   :      0        Min.   :  0.03124  
    ##  1st Qu.:  69372        1st Qu.: 39.36442  
    ##  Median : 222602        Median : 79.72727  
    ##  Mean   : 446300        Mean   : 94.76555  
    ##  3rd Qu.: 523262        3rd Qu.:138.28792  
    ##  Max.   :7990195        Max.   :310.19605

### prepare output data for sensitivity analysis

#### GSA of all maneuvers (total and seperate)

Sum up all forces and distances- for the 4 maneuvers and calculate
output as total friction force per distance This is a scenario where the
track is 10km long and there is 1 corner and 1 accelation or
desceleration event.

    ProbY <- ProbY |> 
      mutate(
        'Total FrWrk factor (J/m)' = (firc_work_acc_dry + 
                                        firc_work_dec_dry +
                                        firc_work_const_dry   +
                                        firc_work_long_corn_dry +
                                        firc_work_lat_corn_dry) / (distance_acc_dry              +
                                                                     distance_dec_dry              +
                                                                     distance_const_dry            +
                                                                     distance_corn_dry             ),
        'total distance (m)' = (distance_acc_dry              +
                                  distance_dec_dry              +
                                  distance_const_dry            +
                                  distance_corn_dry             ),
        'Acceleration FrWrk factor (J/m)' = firc_work_acc_dry/distance_acc_dry,
        'Deceleration FrWrk factor (J/m)' = firc_work_dec_dry/distance_dec_dry,
        'Constant FrWrk factor (J/m)' = firc_work_const_dry/distance_const_dry,
        'Cornering FrWrk factor (J/m)' = (firc_work_long_corn_dry+firc_work_lat_corn_dry)/distance_corn_dry,
        'Cornering FrWrk (J)' = (firc_work_long_corn_dry+firc_work_lat_corn_dry)
      )

    ProbY <- ProbY |> 
      rename(
        `Acceleration FrWrk (J)` = firc_work_acc_dry,
        `Deceleration FrWrk (J)` = firc_work_dec_dry,
        `Constant FrWrk (J)` = firc_work_const_dry
      )


    probX <- input_data |> select(
      # remova collumns that merely intermediates from other varied variables, thereby cluttering the gsa factors
      !(c(alpha_slope, alpha_bank_slope, c_full_brake_ref_tyre, c_accel_max, p_min_acc, p_max_acc,
          sector_velocity_ms, sector_start_velocity_ms, sector_end_velocity_ms, sector_bank_slope,
          sector_underground, sector_alpha_slope,x_slip_long_force_wet_asphalt, c_full_brake_wet_asphalt  ))
    ) |> select(
      # select only input's that are actually varied, otherwise gsa will throw errors.
      where(~ var(.x) != 0)
    )

    # function to simplify the GSA for each manuevre

    GSA_helper <- function(probX, ProbY, FrWrk_type){
      
      
      
      probXY <- bind_cols(
        ProbY |> select(all_of(FrWrk_type)) |> mutate(across(all_of(FrWrk_type), ~ na_if(.x, 0)))
        # transform inputs by adding a constant and taking the logarithm
        ,  probX |> mutate(across(everything(), ~log(.x+100))))  |> 
        # transform output taking logarithm
        mutate( across(all_of(FrWrk_type),~  log(.x))) |> 

        drop_na()
      
      PROB_X= select(probXY,-any_of(FrWrk_type))
      PROB_Y=  pull(probXY,all_of(FrWrk_type))
      
      m1 <- sensiFdiv(model = NULL, X=PROB_X, fdiv = "TV", nboot = 0, conf = 0.95, scale = TRUE)
      tell(m1, y=PROB_Y, S)
      
      borg_df <- tibble(colnames(PROB_X),m1$S$original)
      names(borg_df)<-c("Factor", "delta")
      borg_df$Scenario<- FrWrk_type
      
      borg_df
      
    }

    probX |> summary()

    ##      v_wind           grade_slope         grade_bank_slope    
    ##  Min.   :-4.999737   Min.   :-0.0999696   Min.   :-9.998e-02  
    ##  1st Qu.:-2.488821   1st Qu.:-0.0503498   1st Qu.:-4.999e-02  
    ##  Median :-0.002790   Median : 0.0010824   Median : 6.260e-06  
    ##  Mean   : 0.005924   Mean   : 0.0003473   Mean   : 7.410e-06  
    ##  3rd Qu.: 2.492450   3rd Qu.: 0.0507089   3rd Qu.: 5.000e-02  
    ##  Max.   : 4.999093   Max.   : 0.0999749   Max.   : 1.000e-01  
    ##      c_roll         grip_index_tyre x_correct_road_dry_asphalt
    ##  Min.   :0.001001   Min.   :1.090   Min.   :1.070             
    ##  1st Qu.:0.003258   1st Qu.:1.200   1st Qu.:1.173             
    ##  Median :0.005476   Median :1.317   Median :1.271             
    ##  Mean   :0.005480   Mean   :1.317   Mean   :1.271             
    ##  3rd Qu.:0.007724   3rd Qu.:1.431   3rd Qu.:1.371             
    ##  Max.   :0.009999   Max.   :1.550   Max.   :1.470             
    ##  optimal_slip_dry_asphalt optimal_slip_wet_asphalt   t_0_100kmh   
    ##  Min.   :0.1500           Min.   :0.08000          Min.   : 8.00  
    ##  1st Qu.:0.1621           1st Qu.:0.09009          1st Qu.:10.96  
    ##  Median :0.1749           Median :0.10011          Median :14.05  
    ##  Mean   :0.1748           Mean   :0.10009          Mean   :14.02  
    ##  3rd Qu.:0.1873           3rd Qu.:0.11020          3rd Qu.:17.08  
    ##  Max.   :0.2000           Max.   :0.11999          Max.   :20.00  
    ##    m_vehicle        A_vehicle         c_drag       c_decel_comfort
    ##  Min.   : 900.4   Min.   :2.001   Min.   :0.0200   Min.   :2.000  
    ##  1st Qu.:1415.6   1st Qu.:2.492   1st Qu.:0.1407   1st Qu.:2.254  
    ##  Median :1947.8   Median :2.985   Median :0.2638   Median :2.506  
    ##  Mean   :1949.2   Mean   :2.994   Mean   :0.2615   Mean   :2.501  
    ##  3rd Qu.:2474.2   3rd Qu.:3.494   3rd Qu.:0.3823   3rd Qu.:2.749  
    ##  Max.   :2999.9   Max.   :4.000   Max.   :0.5000   Max.   :3.000  
    ##     r_corner      angle_corner         m_rotate    
    ##  Min.   :10.00   Min.   :  0.0408   Min.   :117.4  
    ##  1st Qu.:20.04   1st Qu.: 88.5176   1st Qu.:198.1  
    ##  Median :30.07   Median :180.7202   Median :272.1  
    ##  Mean   :30.11   Mean   :180.2478   Mean   :272.7  
    ##  3rd Qu.:40.24   3rd Qu.:271.5464   3rd Qu.:346.3  
    ##  Max.   :50.00   Max.   :359.9901   Max.   :448.9  
    ##  frac_driver_comfort_max_accelaration sector_velocity_kmh
    ##  Min.   :0.1003                       Min.   : 0.00815   
    ##  1st Qu.:0.3279                       1st Qu.:17.64756   
    ##  Median :0.5489                       Median :35.42889   
    ##  Mean   :0.5500                       Mean   :35.06699   
    ##  3rd Qu.:0.7756                       3rd Qu.:52.52454   
    ##  Max.   :0.9998                       Max.   :69.99886   
    ##  sector_start_velocity_kmh sector_end_velocity_kmh c_full_brake_dry_asphalt
    ##  Min.   : 0.00292          Min.   : 0.01008        Min.   : 6.258          
    ##  1st Qu.:17.67520          1st Qu.:17.91551        1st Qu.: 8.044          
    ##  Median :35.22088          Median :35.19277        Median : 8.863          
    ##  Mean   :35.08498          Mean   :35.12120        Mean   : 8.938          
    ##  3rd Qu.:52.54252          3rd Qu.:52.65888        3rd Qu.: 9.796          
    ##  Max.   :69.99839          Max.   :69.99977        Max.   :12.130          
    ##  x_slip_long_force_dry_asphalt
    ##  Min.   :3.584e-06            
    ##  1st Qu.:6.360e-06            
    ##  Median :8.187e-06            
    ##  Mean   :9.149e-06            
    ##  3rd Qu.:1.116e-05            
    ##  Max.   :2.714e-05

### Results

    library(sensitivity)

    ## Registered S3 method overwritten by 'sensitivity':
    ##   method    from 
    ##   print.src dplyr

    ## 
    ## Attaching package: 'sensitivity'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     src

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    library(ks)

    FrWrk_type_combi <- c(c('Total FrWrk factor (J/m)','Acceleration FrWrk factor (J/m)','Deceleration FrWrk factor (J/m)', 'Constant FrWrk factor (J/m)','Cornering FrWrk factor (J/m)','Cornering FrWrk (J)','Acceleration FrWrk (J)','Deceleration FrWrk (J)','Constant FrWrk (J)'))

    GSA_output1 <- GSA_helper (probX=probX, ProbY=ProbY, 
                               FrWrk_type=FrWrk_type_combi[1])
    for (i in 2:9){
      GSA_output1 <- bind_rows(GSA_output1,
                               GSA_helper (probX=probX, ProbY=ProbY, 
                                           FrWrk_type=FrWrk_type_combi[i]))
    }


    # png(filename = paste(getwd(),"/figures/Figure sensitivity analysis v2.png",sep=""),
    #     width = 10*0.9, height = 10*0.9, units = "in", res = 300, #pointsize = 50,
    #     bg = "white")
    # 
    # ggplot(GSA_output1, aes(x=Scenario, y=Factor, fill= delta)) + 
    #   geom_tile()+labs(x = "Type of friction work")+
    #   theme(axis.text.y = element_text(size = 8),axis.text.x = element_text(size = 8,angle = 45,hjust = 1)) 
    # 
    # 
    # dev.off()

    # borg_df <- tibble('Factor'=NULL,"delta"=NULL)

    # GSA_output <- foreach(FrWrk_type_d=FrWrk_type_combi,
    #                       .combine = bind_rows,
    #                       .init = ) %dopar%  {
    #                         
    #                         GSA_helper (probX=probX, ProbY=ProbY, 
    #                                     FrWrk_type=FrWrk_type_d)
    #                       }
    # 
    # stopImplicitCluster()
    ggplot(GSA_output1, aes(x=Scenario, y=Factor, fill= delta)) + 
      geom_tile()+labs(x = "Type of friction work")+
      theme(axis.text.y = element_text(size = 8),axis.text.x = element_text(size = 8,angle = 45,hjust = 1)) 

![](Documentation-TWem_files/figure-markdown_strict/sensitivity%20plot-1.png)
