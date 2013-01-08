### THE TRANSPORT SECTOR
### ====================

## FIXME: Includes calculator.R for debugging. The present file should be
## included by a master model file instead

source("../calculator.R")
source("transport-baseline-data.R")

## Transport Activity Driver Tree
## ------------------------------
## 
## <transport> = <passenger transport> + <freight transport>
##
## <pasenger transport> = (No. of people) x <passenger>
##
## <passenger> = (Av. distance per passenger) x [
##                 (car mode share) x <passenger-km-by-car> 
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

## Activities representing a vehicle-km by different modes
## -------------------------------------------------------

## Car | internal combustion engine | 2010 efficiencies

car.ICE.km <- local({
  eff <- efficiency.car.ICE
  Activity(function() {
    list(
      Flow(-eff, direction = "in",
           fuel = fueltype("liquid hydrocarbon"),
           sector = c("car")),
      Flow(eff, direction = "out",
           fuel = fueltype("final demand"),
           sector = c("car")))
  })
})

## Bus | internal combustion engine | 2010 efficiencies

bus.ICE.km <- local({
  eff <- efficiency.bus.ICE
  Activity(function() {
    list(
      Flow(-eff,
           direction = "in",
           fuel = fueltype("liquid hydrocarbon"),
           sector = c("bus")),
      Flow(eff,
           direction = "out",
           fuel = fueltype("final demand"),
           sector = c("bus")))
  })
})




  
      



    
