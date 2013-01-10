### THE TRANSPORT SECTOR
### ====================

source("transport-baseline-data.R")

## Transport Activity Driver Tree
## ------------------------------
## 
## <transport> = <passenger transport> + <freight transport>
##
## <pasenger transport> = (No. of people)
##                         x (Av. distance per passenger)
##                         x <passenger-km>
##
## <passenger-km> =  (car mode share) x <passenger-km-by-car> 
##                 + (bus mode share) x <passenger-km-by-bus> 
##                 + (rail mode share) x <passenger-km-by-rail> 
##                 + (bike mode share) x <passenger-km-by-bike> 
##                 + (walk mode share) x <passenger-km-by-foot> ]
##
## <passenger-km-by-car> = (car occupancy) x <car-km>
##
## <car-km> = (fraction cars with ICE) x <car-ICE-km>
##            + (fraction cars with PHEV) x <car-PHEV-km>
##            + (fraction cars with EV) x <car-EV-km>
##            + (fraction cars with FCV) x <car-FCV-km>
##
## <bus-km> = ...

## make_passenger_transport
##
## Create an activity following the above driver tree, given
## N: Number of people
## d: average distance travelled per person
## mode.share: named vector of mode shares for vehicles
## occupancy: named vector of occupancies for vehicles
## tech.share: named list of vehicles, consisting of a named vector of
## technologies

## make_vehicle_km: vehicle, technology, efficiencies -> Activity
## efficiencies: A list of vehicles, each of which is a list of technologies,
## each of which is a vector of named efficiencies, expresssed in units of [energy]
## [distance]^-1, one for each fuel type.
## Returns an activity representing 1 vehicle-km.

make_vehicle_km <- function(vehicle, technology, efficiencies) {

  eff <- convert(
    efficiencies[[vehicle]][[technology]] * as.Quantity(1, "km"),
    to = "energy")
  
  flows <- mapply(function(x, fuel) {Flow(x, "in", fueltype(fuel), vehicle)},
                  eff, names(eff),
                  USE.NAMES = FALSE)
  
  constant_Activity(c(
    list(Flow(-sum(eff), "out", "final demand", vehicle)),
    flows))
}

## Activites for each vehicle / engine technology choice
## TODO: Could probably automate this ...

vehicle.km <- list(
  car = list(
    ICE = make_vehicle_km("car", "ICE", efficiencies),
    PHEV = make_vehicle_km("car", "PHEV", efficiencies),
    EV = make_vehicle_km("car", "EV", efficiencies)),
  bus = list(
    ICE = make_vehicle_km("bus", "ICE", efficiencies),
    HEV = make_vehicle_km("bus", "HEV", efficiencies),
    EV = make_vehicle_km("bus", "EV", efficiencies)),
  train = list(
    ELECTRIC = make_vehicle_km("train", "ELECTRIC", efficiencies),
    DIESEL = make_vehicle_km("train", "DIESEL", efficiencies)),
  human = list(
    WALK = make_vehicle_km("human", "WALK",
      list(human =
           list(WALK =
                as.Quantity(c(person = 0), "J km^-1"))))))
    
## TODO: Units!

make_transport_passenger <- function(N, d, mode.share, occupancy, tech.share) {
  act.mult(N * d, make_passenger_km(mode.share, occupancy, tech.share)) 
}

make_passenger_km <- function(mode.share, occupancy, tech.share) {
## Ensure all modes known and used

  
}

make_passenger_km_by_mode <- function(vehicle, occupancy, tech.share) {}






  
      



    
