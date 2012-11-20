## Physical constants
## ==================
##
## Calorific values, and emissions factors.
##

## Calorific values
## ----------------
##
## Source: DUKES 2012, Annex A, Estimated average calorific values 2011
## Note: The following are gross calorific values (higher heating value)
## Terminology:
## petrol = Motor spirit (DUKES)
## diesel = DERV (DUKES)

fuel.hhv <- c(petrol = 47.1, diesel = 45.7) # Units: GJ / tonne

## Emissions factors
## -----------------

## Source: DUKES 2012, Annex A, Fuel conversion factors ... 2010

fuel.co2 <- c(gas = 0.185, petrol = 0.240, diesel = 0.252) # kg CO2 / kWh



