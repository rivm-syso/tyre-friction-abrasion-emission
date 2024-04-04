# Estimating the tyre abrasion coefficient

1.  First data on the maneuvers, track, vehicle, tyres and abrassion
    measurements need to be combined into a dataset for use further
    calculations. Then the following calculations are performed:
2.  Total Force at all the tyres together
3.  Total Slip at all tyres together
4.  Calculate total Friction Work for the relevant abrassion measurement
5.  Calculate the Abrasion Coefficient

## 1. Data prepartion

    library(tidyverse)

    ## Warning: package 'ggplot2' was built under R version 4.3.2

    ## Warning: package 'tidyr' was built under R version 4.3.2

    ## Warning: package 'readr' was built under R version 4.3.2

    ## Warning: package 'dplyr' was built under R version 4.3.2

    ## Warning: package 'stringr' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    source("R/Base functions.R")
    local_path <- "data/TWP emission data_IDIADA_v01.xlsx"

    n.Runs = 1000 # for clearly uncertrain variables, amount of values to use

    Maneuver_data <- readxl::read_excel(path = local_path, sheet = "Maneuver data")

    AllData <- 
      Maneuver_data |> 
      full_join(readxl::read_excel(path = local_path, sheet = "Sector data")) |> 
      cross_join(readxl::read_excel(path = local_path, sheet = "Vehicle data")) |> 
      full_join(readxl::read_excel(path = local_path, sheet = "Tyre_data")) |> 
      full_join(readxl::read_excel(path = local_path, sheet = "Scenario data"))

    ## Joining with `by = join_by(Track, `Sector number`)`
    ## Joining with `by = join_by(Vehicle_class)`
    ## Joining with `by = join_by(Track, `Scenario code`)`

    Constants <- readxl::read_excel(path = local_path, sheet = "Constants")
    # str(AllData)
    # AllData$Vehicle_class

    Tyre_Label_table_fuelEff =    readxl::read_excel(
      "data/Tyre_conversion.xlsx", 
      sheet = "Label fuel efficiency class",
      skip=1)
    Tyre_Label_table_wetgrip =    readxl::read_excel(
      "data/Tyre_conversion.xlsx", 
      sheet = "Label wet grip class",
      skip=1)

    AllData2 <-
      AllData |> 
      rowwise() |>  
      mutate(
        RolCoef_min =  fRolCoef_Tlabel(
          Label_fuelleff =
            `Fuel efficiency class`,
          Vehicle_class = Vehicle_class,
          Tyre_Label_table =
            Tyre_Label_table_fuelEff
        )$min,
        RolCoef_max = fRolCoef_Tlabel(
          Label_fuelleff =
            `Fuel efficiency class`,
          Vehicle_class = Vehicle_class,
          Tyre_Label_table = Tyre_Label_table_fuelEff
        )$max,
        GripIndex_min  = fGripIndex_Tlabel(
          Label_wetgrip = `Wet grip class`,
          Vehicle_class = Vehicle_class,
          Tyre_Label_table = Tyre_Label_table_wetgrip
        )$min,
        GripIndex_max = fGripIndex_Tlabel(
          Label_wetgrip = `Wet grip class`,
          Vehicle_class = Vehicle_class,
          Tyre_Label_table = Tyre_Label_table_wetgrip
        )$max
      )
    # create n.Runs number of Rol Coefficients based on uncertainty of tyre label rol coefficient classes
    AllData3 <-
      AllData2 |> mutate(
        RolCoef_u = list(runif(n.Runs, RolCoef_min, RolCoef_max)),
        GripIndex_u = list(runif(n.Runs, GripIndex_min, GripIndex_max)),
        
        x_correct_mu_max_track_u =
          switch(
            Underground,
            "Wet asphalt" = 1,
            "Dry asphalt" = list(
              runif(
                n.Runs,
                min =  Constants |>
                  filter(Name == "x_correct_mu_max_wet2dry_min") |>
                  pull(value),
                max =  Constants |>
                  filter(Name == "x_correct_mu_max_wet2dry_max") |>
                  pull(value)
              )
            )
          ),
        
        optimal_slip_ratio_track_u =
          switch(
            Underground,
            "Wet asphalt" = list(
              runif(
                n.Runs,
                min =  Constants |>
                  filter(Name == "optimal_slip_wet_min") |>
                  pull(value),
                max =  Constants |>
                  filter(Name == "optimal_slip_wet_max") |>
                  pull(value)
              )
            ),
            "Dry asphalt" = list(
              runif(
                n.Runs,
                min =  Constants |>
                  filter(Name == "optimal_slip_dry_min") |>
                  pull(value),
                max =  Constants |>
                  filter(Name == "optimal_slip_dry_max") |>
                  pull(value)
              )
            )
          )
      )


    ### end data prep ###

## 2. Total Force

Longitutidal and Lattidunal forces are calculated

    Force <-
      AllData3 |> mutate(
        ForceDecelLong =
          list(
            f_decel_long_force(
              c_roll = RolCoef_u,
              m_vehicle = `Mass (kg)`,
              grav_constant = Constants |>
                filter(Name == "grav_constant") |> pull(value),
              c_drag = `Aero_drag_coef (-)`,
              A_vehicle = `Surface_Area (m2)`,
              rho_air = Constants |>
                filter(Name == "rho_air") |> pull(value),
              v_start_decel = `Start speed (m/s)`,
              v_end_decel = `End speed (m/s)`,
              v_wind = v_wind,
              alpha_slope = `Longitudinal slope (%)` / 100,
              m_rotate = runif(
                n.Runs,
                Constants |>
                  filter(Name == "min_rotating_fraction") |> pull(value),
                Constants |>
                  filter(Name == "max_rotating_fraction") |> pull(value)
              ),
              c_decel = `Deceleration constant (m.s^-2)`
            )
          ),
        ForceAccelLong =
          list(
            f_accel_long_force(
              c_roll = RolCoef_u,
              m_vehicle = `Mass (kg)`,
              grav_constant = Constants |>
                filter(Name == "grav_constant") |> pull(value),
              c_drag = `Aero_drag_coef (-)`,
              A_vehicle = `Surface_Area (m2)`,
              rho_air = Constants |>
                filter(Name == "rho_air") |> pull(value),
              v_start_accel = `Start speed (m/s)`,
              v_end_accel = `End speed (m/s)`,
              v_wind = v_wind,
              alpha_slope = `Longitudinal slope (%)` / 100,
              m_rotate = runif(
                n.Runs,
                Constants |>
                  filter(Name == "min_rotating_fraction") |> pull(value),
                Constants |>
                  filter(Name == "max_rotating_fraction") |> pull(value)
              ),
              c_accel = `Acceleration constant (m.s^-2)`
            )
          ),
        ForceConstLong =
          list(
            f_const_speed_long_force(
              c_roll = RolCoef_u,
              m_vehicle = `Mass (kg)`,
              grav_constant = Constants |>
                filter(Name == "grav_constant") |> pull(value),
              c_drag = `Aero_drag_coef (-)`,
              A_vehicle = `Surface_Area (m2)`,
              rho_air = Constants |>
                filter(Name == "rho_air") |> pull(value),
              v_vehicle = `End speed (m/s)`,
              v_wind = `v_wind`,
              alpha_slope = `Longitudinal slope (%)` /100)
          ),
        ForceCornLatt_M =
          list(
            f_lat_force( m_vehicle = `Mass (kg)`,
                         grav_constant = Constants |> 
                           filter(Name == "grav_constant") |> pull(value),
                         r_corner = `Corner radius (m)`,
                         alpha_bank_slope = `Latitudinal slope (%)` / 100,
                         v_vehicle = mean(`Start speed (m/s)`,`End speed (m/s)`)) 
          ),
        ForceCornLatt_C =
          list(
            f_lat_force( m_vehicle = `Mass (kg)`,
                         grav_constant = Constants |> 
                           filter(Name == "grav_constant") |> pull(value),
                         r_corner = `Corner radius (m)`,
                         alpha_bank_slope = `Latitudinal slope (%)` / 100,
                         v_vehicle = `End speed (m/s)`) 
          )
      )

## 3. Total Slip

    # add slipt to the data:

    Slip <-
      Force |> mutate(
        SlipDecelLong =
          list(
            f_decel_long_slip(c_roll = RolCoef_u, 
                              m_vehicle = `Mass (kg)`, 
                              grav_constant = Constants |> 
                                filter(Name == "grav_constant") |> pull(value), 
                              c_drag = `Aero_drag_coef (-)`, 
                              A_vehicle = `Surface_Area (m2)`,
                              rho_air = Constants |> 
                                filter(Name == "rho_air") |> pull(value),
                              v_start_decel = `Start speed (m/s)`, 
                              v_end_decel = `End speed (m/s)`, 
                              v_wind = `v_wind`,
                              alpha_slope = `Longitudinal slope (%)`/100, 
                              m_rotate = runif(n.Runs,
                                               Constants |> 
                                                 filter(Name == "min_rotating_fraction") |> 
                                                 pull(value),
                                               Constants |> 
                                                 filter(Name == "max_rotating_fraction") |> 
                                                 pull(value)), 
                              c_decel = `Deceleration constant (m.s^-2)`, 
                              optimal_slip_ratio_tyre_track = optimal_slip_ratio_track_u,
                              grip_index_tyre = GripIndex_u,
                              wet_mu_max_ref_tyre = Constants |>
                                filter(Name == "mu_max_ref_tyre_wet") |> pull(value),
                              x_correct_mu_max_track = x_correct_mu_max_track_u,
                              c_brake_ref_tyre_wet = Constants |> 
                                filter(Name == "c_brake_ref_tyre_wet") |> pull(value)
            ) 
          ),
        SlipAccelLong= list(
          f_accel_long_slip(c_roll = RolCoef_u, 
                            m_vehicle = `Mass (kg)`, 
                            grav_constant = Constants |> 
                              filter(Name == "grav_constant") |> pull(value), 
                            c_drag = `Aero_drag_coef (-)`, 
                            A_vehicle = `Surface_Area (m2)`, 
                            rho_air = Constants |> 
                              filter(Name == "rho_air") |> pull(value),
                            v_start_accel = `Start speed (m/s)`, 
                            v_end_accel = `End speed (m/s)`, 
                            v_wind = v_wind, 
                            alpha_slope = `Longitudinal slope (%)`/100, 
                            m_rotate = runif(n.Runs,
                                             Constants |> 
                                               filter(Name == "min_rotating_fraction") |> 
                                               pull(value),
                                             Constants |> 
                                               filter(Name == "max_rotating_fraction") |> 
                                               pull(value)), 
                            c_accel = `Acceleration constant (m.s^-2)`, 
                            optimal_slip_ratio_tyre_track = optimal_slip_ratio_track_u,
                            grip_index_tyre = GripIndex_u, 
                            wet_mu_max_ref_tyre = Constants |> 
                              filter(Name == "mu_max_ref_tyre_wet") |> pull(value),
                            x_correct_mu_max_track = x_correct_mu_max_track_u)
        ),
        SlipConstLong = 
          list(
            f_const_speed_long_slip(c_roll = RolCoef_u, 
                                    m_vehicle = `Mass (kg)`, 
                                    m_rotate = runif(n.Runs,
                                                     Constants |> 
                                                       filter(Name == "min_rotating_fraction") |> 
                                                       pull(value),
                                                     Constants |> 
                                                       filter(Name == "max_rotating_fraction") |> 
                                                       pull(value)),
                                    grav_constant = Constants |> 
                                      filter(Name == "grav_constant") |> pull(value), 
                                    c_drag = `Aero_drag_coef (-)`, 
                                    A_vehicle = `Surface_Area (m2)`, 
                                    rho_air = Constants |> 
                                      filter(Name == "rho_air") |> pull(value), 
                                    v_vehicle = `End speed (m/s)`, 
                                    v_wind = `v_wind`,
                                    alpha_slope = `Longitudinal slope (%)`/100,
                                    wet_mu_max_ref_tyre = Constants |> 
                                      filter(Name == "mu_max_ref_tyre_wet") |> pull(value), 
                                    optimal_slip_ratio_tyre_track = optimal_slip_ratio_track_u,
                                    grip_index_tyre = GripIndex_u, 
                                    c_brake_ref_tyre_wet = Constants |> 
                                      filter(Name == "c_brake_ref_tyre_wet") |> pull(value), 
                                    x_correct_mu_max_track = x_correct_mu_max_track_u)
          ),
        SlipCornLatt_M =
          list(
            f_lat_slip(m_vehicle = `Mass (kg)`, 
                       v_vehicle = mean(`Start speed (m/s)`,
                                        `End speed (m/s)`), 
                       r_corner = `Corner radius (m)`, 
                       grav_constant = Constants |> 
                         filter(Name == "grav_constant") |> pull(value),
                       alpha_bank_slope = `Latitudinal slope (%)`/100, 
                       optimal_slip_ratio_tyre_track = optimal_slip_ratio_track_u,
                       grip_index_tyre = GripIndex_u, 
                       wet_mu_max_ref_tyre = Constants |> 
                         filter(Name == "mu_max_ref_tyre_wet") |> pull(value), 
                       x_correct_mu_max_track = x_correct_mu_max_track_u)
          ),
        SlipCornLatt_C =
          list(
            f_lat_slip(m_vehicle = `Mass (kg)`, 
                       v_vehicle = `End speed (m/s)`, 
                       r_corner = `Corner radius (m)`, 
                       grav_constant = Constants |> 
                         filter(Name == "grav_constant") |> pull(value),
                       alpha_bank_slope = `Latitudinal slope (%)`/100, 
                       optimal_slip_ratio_tyre_track = optimal_slip_ratio_track_u,
                       grip_index_tyre = GripIndex_u, 
                       wet_mu_max_ref_tyre = Constants |> 
                         filter(Name == "mu_max_ref_tyre_wet") |> pull(value), 
                       x_correct_mu_max_track = x_correct_mu_max_track_u)
          ) 
      )

## 4. Total Friction Work

    TotalFW <- 
      Slip |> mutate(
        DistanceManAccel =
          f_accel_distance(v_start = `Start speed (m/s)`,
                           v_end = `End speed (m/s)`,
                           c_accel = `Acceleration constant (m.s^-2)`),
        DistanceManDecel = 
          f_decel_distance(v_start = `Start speed (m/s)`,
                           v_end = `End speed (m/s)`,
                           c_decel = `Deceleration constant (m.s^-2)`),
        DistanceCorn = f_corner_distance(r_corner = `Corner radius (m)`,
                                         corner_angle = `Corner angle (degrees)`)
      )

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `DistanceManDecel = f_decel_distance(...)`.
    ## ℹ In row 21.
    ## Caused by warning in `f_decel_distance()`:
    ## ! acceleration function needed

    FrictionWork = 
      TotalFW |> mutate(DistanceConst = `Distance (m)`-
                          DistanceManAccel*`Maneuver repeats` - 
                          DistanceManDecel*`Maneuver repeats`,
                        FWAccelLong = list(ForceAccelLong*SlipAccelLong*DistanceManAccel*`Maneuver repeats`),
                        FWDecelLong = list(ForceDecelLong*SlipDecelLong*DistanceManDecel*`Maneuver repeats`),
                        FWConstLong = list(ForceConstLong*SlipConstLong*DistanceConst),
                        FWLat = list(
                          SlipCornLatt_M*ForceCornLatt_M*(DistanceManAccel+DistanceManDecel) +
                          SlipCornLatt_C*ForceCornLatt_C*DistanceConst),
                        TotalFricWork = list((FWAccelLong+FWDecelLong+FWConstLong+FWLat)*`Number of laps`),
                        .keep = c("used")
                        
      )
