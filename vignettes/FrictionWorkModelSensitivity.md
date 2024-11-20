# Estimating the tyre abrasion coefficient

1.  First data on the maneuvers, track, vehicle, tyres and abrassion
    measurements need to be combined into a dataset for use further
    calculations. Then the following calculations are performed:
2.  Total Force at all the tyres together
3.  Total Slip at all tyres together
4.  Calculate total Friction Work for the relevant abrassion measurement
5.  Perform Global Sensitivity Analysis

## 1. Data prepartion

    ## # A tibble: 4,000 × 34
    ##    `Section name` `Test section` AbrasionTest  Track            `Number of laps`
    ##    <chr>          <chr>          <chr>         <chr>                       <dbl>
    ##  1 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  2 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  3 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  4 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  5 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  6 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  7 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  8 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  9 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ## 10 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ## # ℹ 3,990 more rows
    ## # ℹ 29 more variables: `Total distance (km)` <dbl>, RolCoef_u <dbl>,
    ## #   GripIndex_u <dbl>, Underground <chr>, x_correct_mu_max_track_u <dbl>,
    ## #   optimal_slip_ratio_track_u <dbl>, RUNS <int>, `Mass (kg)` <dbl>,
    ## #   `Aero_drag_coef (-)` <dbl>, `Surface_Area (m2)` <dbl>,
    ## #   `Longitudinal slope (%)` <dbl>, `Latitudinal slope (%)` <dbl>,
    ## #   v_wind <dbl>, m_rotate <dbl>, v_start_decel <dbl>, v_end_decel <dbl>, …

## 2. Total Force

Longitutidal and Lattidunal forces are calculated

    ## # A tibble: 6 × 36
    ## # Rowwise: 
    ##   `Section name` `Test section` AbrasionTest  Track             `Number of laps`
    ##   <chr>          <chr>          <chr>         <chr>                        <dbl>
    ## 1 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 2 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 3 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 4 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 5 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## 6 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer LaneS…                1
    ## # ℹ 31 more variables: `Total distance (km)` <dbl>, RolCoef_u <dbl>,
    ## #   GripIndex_u <dbl>, Underground <chr>, x_correct_mu_max_track_u <dbl>,
    ## #   optimal_slip_ratio_track_u <dbl>, RUNS <int>, `Mass (kg)` <dbl>,
    ## #   `Aero_drag_coef (-)` <dbl>, `Surface_Area (m2)` <dbl>,
    ## #   `Longitudinal slope (%)` <dbl>, `Latitudinal slope (%)` <dbl>,
    ## #   v_wind <dbl>, m_rotate <dbl>, v_start_decel <dbl>, v_end_decel <dbl>,
    ## #   v_start_accel <dbl>, v_end_accel <dbl>, v_const <dbl>, …

## 3. Total Slip

    ## # A tibble: 4,000 × 38
    ## # Rowwise: 
    ##    `Section name` `Test section` AbrasionTest  Track            `Number of laps`
    ##    <chr>          <chr>          <chr>         <chr>                       <dbl>
    ##  1 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  2 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  3 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  4 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  5 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  6 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  7 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  8 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ##  9 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ## 10 MotorwaySIML   Motorway_SIML  Motorway_SIML Oval Outer Lane…                1
    ## # ℹ 3,990 more rows
    ## # ℹ 33 more variables: `Total distance (km)` <dbl>, RolCoef_u <dbl>,
    ## #   GripIndex_u <dbl>, Underground <chr>, x_correct_mu_max_track_u <dbl>,
    ## #   optimal_slip_ratio_track_u <dbl>, RUNS <int>, `Mass (kg)` <dbl>,
    ## #   `Aero_drag_coef (-)` <dbl>, `Surface_Area (m2)` <dbl>,
    ## #   `Longitudinal slope (%)` <dbl>, `Latitudinal slope (%)` <dbl>,
    ## #   v_wind <dbl>, m_rotate <dbl>, v_start_decel <dbl>, v_end_decel <dbl>, …

## 4. Total Friction Work

    ## # A tibble: 990 × 10
    ## # Groups:   RUNS [990]
    ##     RUNS `Test section` FrictionWork_Long FrictionWork_Latt Distance_Track
    ##    <int> <chr>                      <dbl>             <dbl>          <dbl>
    ##  1     1 Motorway_SIML            230359.            50333.          9425.
    ##  2     2 Motorway_SIML            115251.             7719.          9425.
    ##  3     3 Motorway_SIML            415108.            29341.          9425.
    ##  4     4 Motorway_SIML             90094.             9101.          9425.
    ##  5     5 Motorway_SIML           2963840.            43386.          9425.
    ##  6     6 Motorway_SIML            228548.            56120.          9425.
    ##  7     7 Motorway_SIML            649426.            39321.          9425.
    ##  8     8 Motorway_SIML            399459.            53259.          9425.
    ##  9     9 Motorway_SIML            329107.            37303.          9425.
    ## 10    10 Motorway_SIML            452990.            12604.          9425.
    ## # ℹ 980 more rows
    ## # ℹ 5 more variables: FrictionWork_Long_pm <dbl>, FrictionWork_Latt_pm <dbl>,
    ## #   FrictionWork_pm <dbl>, FrictionWork_Latt_7x_pm <dbl>,
    ## #   FrictionWork_7x_pm <dbl>

## 5. Global Sensitivity Analysis

![](FrictionWorkModelSensitivity_files/figure-markdown_strict/unnamed-chunk-2-1.png)
