# Calculations

Maneuver_data <- readxl::read_excel(path = local_path, sheet = "Maneuver data")

AllData <- 
  Maneuver_data |> 
  full_join(readxl::read_excel(path = local_path, sheet = "Sector data")) |> 
  cross_join(readxl::read_excel(path = local_path, sheet = "Vehicle data")) |> 
  full_join(readxl::read_excel(path = local_path, sheet = "Tyre_data"),relationship = "many-to-many") |> 
  full_join(readxl::read_excel(path = local_path, sheet = "Test data"))

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

AllData <-
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
AllData <-
  AllData |> mutate(
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
