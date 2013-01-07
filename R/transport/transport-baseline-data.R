### Transport baseline datasets
### ===========================

### Defines:
### historic.vehicle.efficiency -- a dataframe
### car.efficiency.2010
### bus.efficiency.2010
### hgv.efficiency.2010

## METHOD
##
## 2010 car, bus, and hgv ICE efficiencies taken from 2010 total transport
## figures as published by DfT.

## TODO: At present, it is intended that this file be source'd by the main
## calculator code. However, it may be nicer to write this file as R markdown,
## and have it processed by knitr. The output would be (a) an RData file
## containing the required baseline data; and (b) a nicely-formatted report
## explaining what we did.

library(plyr)
library(siunits)

### Road Vehicle Efficiencies
### =========================

data("env0101") # Fuel consumption by vehicle type (from TSGB 2012)
data("tra0201") # Vehicle distances by vehicle type (from TSGB 2012)
data("calorific-values") # Calorific content of fuels (from DUKES 2012)

## Note: "car" includes cars, taxis, motorcycles, mopeds, and light vans.
## Problems: Light vans are used both for passengers and for freight; the
## overall "car" efficiency for passenger transport will therefore be
## overweighted by vans. 

## The names of fuels as used in TSGB are not the same as those used in DUKES
## Annex A.1, which have anyway been rewritten to be more R-friendly. The names
## of vehicles in different TSGB tables are not necessarily consistent. 

fuel.lookup <- read.table(header = TRUE, text = "
tsgb                    dukes
'Aviation spirit'       aviation.spirit
'Aviation turbine fuel' aviation.turbine.fuel
'Diesel'                diesel
'Fuel oils'             fuel.oil
'Gas oil'               gas.oil
'LPG'                   lpg
'Petrol'                petrol")

env0101.vehicles <- read.table(header = TRUE, text = "
tsgb                    calc 
'Buses & coaches'       bus
'Cars & taxis'          car
'Heavy goods vehicles'  hgv
'Light vans'            car
'Motorcycles & mopeds'  car")

tra0201.vehicles <- read.table(header = TRUE, text = "
tsgb               calc
'Cars and taxis'   car
'Motorcycles'      car
'Buses & coaches'  bus
'Light vans'       car
'Goods vehicles'   hgv
'Pedal cycles'     bike")

## Convert fuel consumption to energy
tsgb.env0101$dukes.fuel <- fuel.lookup$dukes[match(tsgb.env0101$fuel,
                                                   fuel.lookup$tsgb)]

tsgb.env0101$energy <- tsgb.env0101$consumption * dukes.annex.A.1[tsgb.env0101$dukes.fuel]

## Convert vehicle names to common names
tsgb.env0101$calc.vehicle <- env0101.vehicles$calc[match(tsgb.env0101$vehicle,
                                                         env0101.vehicles$tsgb)]

tsgb.tra0201$calc.vehicle <- tra0201.vehicles$calc[match(tsgb.tra0201$vehicle,
                                                         tra0201.vehicles$tsgb)]

## Summarise distance and fuel consumption by vehicle type and year

fuel <- ddply(tsgb.env0101, .(calc.vehicle, yr), summarise, energy = sum(energy))
dist <- ddply(tsgb.tra0201, .(calc.vehicle, Year), summarise, distance = sum(distance))
dist$yr <- dist$Year
dist$Year <- NULL

## Combine the two datasets
historic.vehicle.efficiency <- transform(merge(dist, fuel),
                                         eff = as.Quantity(energy / distance,
                                           "(kW h)_[energy] km^-1"))
                                           
car.efficiency.2010 <- with(historic.vehicle.efficiency,
                            eff[calc.vehicle == "car" & yr == 2010])

bus.efficiency.2010 <- with(historic.vehicle.efficiency,
                            eff[calc.vehicle == "bus" & yr == 2010])

hgv.efficiency.2010 <- with(historic.vehicle.efficiency,
                            eff[calc.vehicle == "hgv" & yr == 2010])








     
