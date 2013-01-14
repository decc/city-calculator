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

## make_vehicle_km: vehicle, technology, efficiencies -> Activity
## efficiencies: A list of vehicles, each of which is a list of technologies,
## each of which is a vector of named efficiencies, expresssed in units of [energy]
## [distance]^-1, one for each fuel type.
## Returns an activity representing 1 vehicle-km.

vehicle_technology_km_model <- function(vehicle, technology, efficiencies) {
  eff <- convert(
    efficiencies[[vehicle]][[technology]] * as.Quantity(1, "km"),
    to = "energy")
  
  flows <- mapply(function(x, fuel) {Flow(x, "in", fueltype(fuel), vehicle)},
                  eff, names(eff),
                  SIMPLIFY = FALSE,
                  USE.NAMES = FALSE)


  make_constant_Activity(c(list(Flow(-sum(eff), "out", "final demand", vehicle)),
                           flows))
}

## Activites for each vehicle / engine technology choice
## TODO: Could probably automate this ...

vehicle.km <- list(
  car = list(
    ICE = vehicle_technology_km_model("car", "ICE", efficiencies),
    PHEV = vehicle_technology_km_model("car", "PHEV", efficiencies),
    EV = vehicle_technology_km_model("car", "EV", efficiencies)),
  bus = list(
    ICE = vehicle_technology_km_model("bus", "ICE", efficiencies),
    HEV = vehicle_technology_km_model("bus", "HEV", efficiencies),
    EV = vehicle_technology_km_model("bus", "EV", efficiencies)),
  train = list(
    ELECTRIC = vehicle_technology_km_model("train", "ELECTRIC", efficiencies),
    DIESEL = vehicle_technology_km_model("train", "DIESEL", efficiencies)),
  human = list(
    WALK = vehicle_technology_km_model("human", "WALK",
      list(human =
           list(WALK =
                as.Quantity(c(person = 0), "J km^-1"))))))
    
## make_passenger_transport
##
## Create an activity following the above driver tree, given
## N: Number of people
## d: average distance travelled per person
## mode.share: named vector of mode shares for vehicles
## occupancy: named vector of occupancies for vehicles
## tech.share: named list of vehicles, consisting of a named vector of
## technologies

transport_passenger_model <- function(N, d, mode.share, occupancy, tech.share) {
  act.scalar_multiply(N * d,
                  passenger_km_model(mode.share, occupancy, tech.share)) 
  ## Add sector label?
  
}

passenger_km_model <- function(mode.share, occupancy, tech.share) {
  ## For each mode in mode.share, make a vehicle.km
  act.add( 
    lapply(names(mode.share),
           function(mode) {
             act.scalar_multiply(mode.share[[mode]] * occupancy[[mode]],
                                 vehicle_km_model(mode, tech.share))}))
}

vehicle_km_model <- function(mode, tech.share) {
  ## Combine the different kinds of technology for a given mode
  act.add(
    lapply(names(tech.share[[mode]]),
           function(tech) {
             act.scalar_multiply(tech.share[[mode]][[tech]],
                                 vehicle.km[[mode]][[tech]])}))
}
      



  
      



    
