### Transport baseline datasets
### ===========================

## TODO: At present, it is intended that this file be source'd by the main
## calculator code. However, it may be nicer to write this file as R markdown,
## and have it processed by knitr. The output would be (a) an RData file
## containing the required baseline data; and (b) a nicely-formatted report
## explaining what we did.

## TODO: Allow baseline efficiency data to be requested for any given year.

library(plyr)
library(siunits)

### Output
### ------
###
### All efficiencies refer to 2010
###
### Computed from current data:
###
### historic.vehicle.efficiency -- a dataframe
### efficiency.car.ICE
### efficiency.bus.ICE
### efficiency.hgv.ICE
###
### Taken from the 2050 Calculator:
### (Downloaded 2013-01-08)
###
### efficiency.car.EV
### efficiency.car.PHEV.petrol
### efficiency.car.PHEV.electricity
### efficiency.bus.ICE
### efficiency.train.DIESEL
### efficiency.train.ELECTRIC

## 2050 CALCULATOR DATA

efficiencies <- list(
  car = list(
    EV = as.Quantity(c(electricity = 600), "TJ Tm^-1"),
    PHEV = as.Quantity(
      c("liquid hydrocarbon" = 508,
                 electricity = 417), "TJ Tm^-1")),
  bus = list(
    HEV = as.Quantity(c("liquid hydrocarbon" = 9885), "TJ Tm^-1"),
    EV = as.Quantity(c(electricity = 3361), "TJ Tm^-1")), 
  train = list(
    DIESEL = as.Quantity(c("liquid hydrocarbon" = 0.11),
      "(TW h)_[energy] Tm^-1"), # per seat-km
    ELECTRIC = as.Quantity(c(electricity = 0.032),
      "(TW h)_[energy] Tm^-1"))) # per seat-km


## CALCULATED EFFICIENCIES 
##
## 2010 car, bus, and hgv ICE efficiencies taken from 2010 total transport
## figures as published by DfT.

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

## Fix inconsistent naming conventions
dist$yr <- dist$Year 
dist$Year <- NULL

## Combine the two datasets
historic.vehicle.efficiency <- transform(merge(dist, fuel),
                                         eff = as.Quantity(energy / distance,
                                           "(kW h)_[energy] km^-1"))

## Add in computed efficiencies to the output table
efficiencies[["car"]][["ICE"]] <-
  with(historic.vehicle.efficiency,
       structure(eff[calc.vehicle == "car" & yr == 2010], names = "liquid hydrocarbon"))

efficiencies[["bus"]][["ICE"]] <-
  with(historic.vehicle.efficiency,
       structure(eff[calc.vehicle == "bus" & yr == 2010], names = "liquid hydrocarbon"))

efficiencies[["hgv"]] <-
  list(ICE = 
       with(historic.vehicle.efficiency,
            structure(eff[calc.vehicle == "hgv" & yr == 2010], names = "liquid hydrocarbon")))
       







     
