## THE TRANSPORT SECTOR
##

source("calculator.R")

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

## 2010 Vehicle efficiencies
## -------------------------

