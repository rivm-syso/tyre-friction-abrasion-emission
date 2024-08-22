AllData <-
  AllData |> mutate(
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
