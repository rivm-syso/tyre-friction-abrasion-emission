AllData <-
  AllData |> mutate(
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

