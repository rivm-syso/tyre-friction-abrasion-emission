
# library(readxl)

# Tyre_Label_table <- readxl::read_excel("data/Tyre_conversion.xlsx", sheet = "Label fuel efficiency class",skip=1)

fRolCoef_Tlabel <- function(Label_fuelleff="A", # A, B, C, D, E
                            vehicle_class="C1",
                            Tyre_Label_table) { # C1, C2, C3 

# Tyre_Label_table  <- 
  dataprep <- Tyre_Label_table |> pivot_longer(
    cols = C1_min:C3_max,
    names_to = "trclass_min_max",
    values_to = "roll coefficient"
  ) |> separate(
    col = trclass_min_max,
    into = c("Vehicle class", "Class Min/Max"),
    sep = "_"
  ) 
 
  list(min= dataprep |> filter(`Vehicle class` == vehicle_class & 
                                    `Class Min/Max` == "min" &  
                                    `Label_fuel_efficiency_class` == Label_fuelleff) |> pull( `roll coefficient`),
       max = dataprep|> filter(`Vehicle class` == vehicle_class & 
                                 `Class Min/Max` == "max" &  
                                 `Label_fuel_efficiency_class` == Label_fuelleff) |> pull( `roll coefficient`))
   
}

fGripIndex_Tlabel <- function(Label_wetgrip, # A, B, C, D, E
                            vehicle_class,
                            Tyre_Label_table) { # C1, C2, C3 
  
  # Tyre_Label_table  <- 
  dataprep <- Tyre_Label_table |> pivot_longer(
    cols = C1_min:C3_max,
    names_to = "trclass_min_max",
    values_to = "Grip Index"
  ) |> separate(
    col = trclass_min_max,
    into = c("Vehicle class", "Class Min/Max"),
    sep = "_"
  ) 
  
  list(min= dataprep |> filter(`Vehicle class` == vehicle_class & 
                                 `Class Min/Max` == "min" &  
                                 `Label_wet_grip_class` == Label_wetgrip) |> pull( `Grip Index`),
       max = dataprep|> filter(`Vehicle class` == vehicle_class & 
                                 `Class Min/Max` == "max" &  
                                 `Label_wet_grip_class` == Label_wetgrip) |> pull( `Grip Index`))
  
}
