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

## Create an activity following the above driver tree, given
## N: Number of people
## d: average distance travelled per person
## mode.share: named vector of mode shares for vehicles
## occupancy: named vector of occupancies for vehicles
## tech.share: named list of vehicles, consisting of a named vector of
## technologies

passenger_transport.model <- function(N, d, mode.share, occupancy, tech.share) {
  activity.scalar_multiply(N * d,
                  passenger_km.model(mode.share, occupancy, tech.share)) 
  ## Add sector label?
  
}

passenger_km.model <- function(mode.share, occupancy, tech.share) {
  ## For each mode in mode.share, make a vehicle.km
  activity.add( 
    lapply(names(mode.share),
           function(mode) {
             activity.scalar_multiply(mode.share[[mode]] * occupancy[[mode]],
                                 vehicle_km.model(mode, tech.share))}))
}

vehicle_km.model <- function(mode, tech.share) {
  ## Combine the different kinds of technology for a given mode
  activity.add(
    lapply(names(tech.share[[mode]]),
           function(tech) {
             activity.scalar_multiply(tech.share[[mode]][[tech]],
                                 vehicle.km[[mode]][[tech]])}))
}
      
vehicle_technology_km.model <- function(vehicle, technology, efficiencies) {
  eff <- convert(
    efficiencies[[vehicle]][[technology]] * as.Quantity(1, "km"),
    to = "energy")
  
  flows <- mapply(function(x, fuel) {Flow(x, "in", fueltype(fuel), vehicle)},
                  eff, names(eff),
                  SIMPLIFY = FALSE,
                  USE.NAMES = FALSE)


  make_constant_Activity(c(list(Flow(-sum(eff), "out", fueltype("final demand"),
                           vehicle)), flows))
}

## Activites for each vehicle / engine technology choice
## TODO: Could probably automate this ...

efficiencies.human <- list(human =
                           list(WALK = as.Quantity(c(person = 0), "J km^-1"),
                                BIKE = as.Quantity(c(person = 0), "J km^-1")))

vehicle.km <- list(
  car = list(
    ICE = vehicle_technology_km.model("car", "ICE", efficiencies),
    PHEV = vehicle_technology_km.model("car", "PHEV", efficiencies),
    EV = vehicle_technology_km.model("car", "EV", efficiencies)),
  bus = list(
    ICE = vehicle_technology_km.model("bus", "ICE", efficiencies),
    HEV = vehicle_technology_km.model("bus", "HEV", efficiencies),
    EV = vehicle_technology_km.model("bus", "EV", efficiencies)),
  train = list(
    ELECTRIC = vehicle_technology_km.model("train", "ELECTRIC", efficiencies),
    DIESEL = vehicle_technology_km.model("train", "DIESEL", efficiencies)),
  human = list(
    WALK = vehicle_technology_km.model("human", "WALK", efficiencies.human),
    BIKE = vehicle_technology_km.model("human", "BIKE", efficiencies.human)))

  
      



    
