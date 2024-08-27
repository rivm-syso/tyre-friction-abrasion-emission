# Estimating the tyre abrasion coefficient

1.  First data on the maneuvers, track, vehicle, tyres and abrassion
    measurements need to be combined into a dataset for use further
    calculations. Then the following calculations are performed:
2.  Total Force at all the tyres together
3.  Total Slip at all tyres together
4.  Calculate total Friction Work for the relevant abrassion measurement
5.  Perform Global Sensitivity Analysis

## 1. Data prepartion

    ## # A tibble: 6 × 34
    ##   `Section name` `Test section` AbrasionTest  Track             `Number of laps`
    ##   <chr>          <chr>          <chr>         <chr>                        <dbl>
    ## 1 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 2 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 3 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 4 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 5 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 6 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## # ℹ 29 more variables: `Total distance (km)` <dbl>, RolCoef_u <dbl>,
    ## #   GripIndex_u <dbl>, Underground <chr>, x_correct_mu_max_track_u <dbl>,
    ## #   optimal_slip_ratio_track_u <dbl>, RUNS <int>, `Mass (kg)` <dbl>,
    ## #   `Aero_drag_coef (-)` <dbl>, `Surface_Area (m2)` <dbl>,
    ## #   `Longitudinal slope (%)` <dbl>, `Latitudinal slope (%)` <dbl>,
    ## #   v_wind <dbl>, m_rotate <dbl>, v_start_decel <dbl>, v_end_decel <dbl>,
    ## #   v_start_accel <dbl>, v_end_accel <dbl>, v_const <dbl>, …

## 2. Total Force

Longitutidal and Lattidunal forces are calculated

## 3. Total Slip

## 4. Total Friction Work

    ## # A tibble: 9 × 41
    ## # Rowwise: 
    ##   `Section name` `Test section` AbrasionTest  Track             `Number of laps`
    ##   <chr>          <chr>          <chr>         <chr>                        <dbl>
    ## 1 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 2 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 3 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 4 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 5 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 6 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 7 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 8 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 9 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## # ℹ 36 more variables: `Total distance (km)` <dbl>, RolCoef_u <dbl>,
    ## #   GripIndex_u <dbl>, Underground <chr>, x_correct_mu_max_track_u <dbl>,
    ## #   optimal_slip_ratio_track_u <dbl>, RUNS <int>, `Mass (kg)` <dbl>,
    ## #   `Aero_drag_coef (-)` <dbl>, `Surface_Area (m2)` <dbl>,
    ## #   `Longitudinal slope (%)` <dbl>, `Latitudinal slope (%)` <dbl>,
    ## #   v_wind <dbl>, m_rotate <dbl>, v_start_decel <dbl>, v_end_decel <dbl>,
    ## #   v_start_accel <dbl>, v_end_accel <dbl>, v_const <dbl>, …

    ## # A tibble: 2 × 1
    ## # Rowwise: 
    ##   `Sector name`     
    ##   <chr>             
    ## 1 East BendSIML     
    ## 2 North StraightSIML

## 5. Global Sensitivity Analysis

![](FrictionWorkModelSensitivity_files/figure-markdown_strict/unnamed-chunk-2-1.png)
