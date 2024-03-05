##
# Input data

## Input vehicle specifications ##
vehicle_name = "Ford Kuga"
#Vehicle mass in kg
m_vehicle = 1660
#Vehicle surface area in m^2
A_vehicle = 2.629
#Vehicle aerodynamic drag coefficient
c_drag = 0.347
# Mass of the rotating parts of the vehicle(kg)
m_rotate = (runif(n = 1000, 0.13, 0.15))*m_vehicle

## Input track-tyre parameters

# minimum roll coefficient (kg/t) according to EU label
c_roll_tyre_min = 9.1
# maximum roll coefficient (kg/t) according to EU label
c_roll_tyre_max = 10.5

# minimum grip index according to EU label
grip_index_tyre_min = 1.55
# maximum grip index according to EU label
grip_index_tyre_max = 1.56



# track underground
track_underground = "dry_asphalt"

## Physics data ##
### Gravitational constant in m.s^-2
grav_constant = 9.81

## Landscape data ##
###Density of air in kg.m^-3
rho_air = 1.205
### Wind speed in m.s^-1
v_wind = runif(n = 1000, -5, 5)



### track-tyre parametrisations

wet_mu_max_ref_tyre = 0.85
c_full_brake_ref_tyre_wet = 0.68*grav_constant
grip_index_tyre = runif(n=1000, min=grip_index_tyre_min, max=grip_index_tyre_max)
c_max_brake = (grip_index_tyre * c_full_brake_ref_tyre_wet) / 1.25


if (track_underground == "dry_asphalt") 
{  optimal_slip_track = runif(n = 1000, 0.15, 20)}

if (track_underground == "dry_asphalt") 
{  x_correct_track = runif(n = 1000, 1.07, 1.47)}

c_roll = (runif(n=1000, min=c_roll_tyre_min, max=c_roll_tyre_max))/1000