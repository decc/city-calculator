### Transport baseline datasets
### Defines:
### vehicle.efficiencies(year), which computes actual car, bus, and hgv efficiencies 

## TODO: At present, it is intended that this file be source'd by the main
## calculator code. However, it may be nicer to write this file as R markdown,
## and have it processed by knitr. The output would be (a) an RData file
## containing the required baseline data; and (b) a nicely-formatted report
## explaining what we did.

library(siunits)

### Vehicle Efficiencies
### ====================

data("env0101") # Fuel consumption by vehicle type (from TSGB 2012)
data("tra0201") # Vehicle distances by vehicle type (from TSGB 2012)
data("calorific-values") # Calorific content of fuels (from DUKES 2012)

## Note: "car" includes cars, taxis, motorcycles, mopeds, and light vans.
## Problems: Light vans are used both for passengers and for freight; the
## overall "car" efficiency for passenger transport will therefore be
## overweighted by vans. 

## Convert fuel mass (as given in TSGB Table env0101) to energy content
## --------------------------------------------------------------------

## The names of fuels as used in TSGB are not the same as those used in DUKES
## Annex A.1, which have anyway been rewritten to be more R-friendly. The
## following is a lookup table to convert one to the other. 

fuel.lookup <- read.table(header = TRUE, text = "
tsgb                    dukes
'Aviation spirit'       aviation.spirit
'Aviation turbine fuel' aviation.turbine.fuel
'Diesel'                diesel
'Fuel oils'             fuel.oil
'Gas oil'               gas.oil
'LPG'                   lpg
'Petrol'                petrol")

tsgb.env0101$dukes.fuel <- fuel.lookup$dukes[match(tsgb.env0101$fuel,
                                                   fuel.lookup$tsgb)]

tsgb.env0101$energy <- tsgb.env0101$consumption * dukes.annex.A.1[tsgb.env0101$dukes.fuel]

vehicle.efficiencies <- function(yy) {  
  ## Compute total distance travelled by vehicle type
  distance <- with(tsgb.tra0201,
                   c(car =
                     sum(distance[Year == yy &
                                  vehicle %in% c("Cars and taxis",
                                                 "Motorcycles",
                                                 "Light vans")]),
                     bus =
                     sum(distance[Year == yy &
                                  vehicle %in% c("Buses & coaches")]),
                     hgv =
                     sum(distance[Year == yy &
                                  vehicle %in% c("Goods vehicles")])))
                   
  ## Compute total energy used, by vehicle type
  energy <- with(tsgb.env0101,
                 c(car =
                   sum(energy[yr == yy &
                              vehicle %in% c("Cars & taxis",
                                             "Light vans",
                                             "Motorcycles & mopeds")]),
                   bus =
                   sum(energy[yr == yy &
                              vehicle %in% c("Buses & coaches")]),
                   hgv =
                   sum(energy[yr == yy &
                              vehicle %in% c("Heavy goods vehicles")])))
  
  ## Compute and return actual efficiencies 
  as.Quantity(c(car = energy[["car"]] / distance[["car"]],
                bus = energy[["bus"]] / distance[["bus"]],
                hgv = energy[["hgv"]] / distance[["hgv"]]),
              "(kW h)_[energy] km^-1")
}







     
