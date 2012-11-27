## Physical constants
## ==================
##
## Calorific values and emissions factors

library(siunits)

## Calorific values
## ----------------
##
## Source: DUKES 2012, Annex A, Estimated average calorific values 2011
## Note: The following are gross calorific values (higher heating value)
## Terminology:
## petrol = Motor spirit (DUKES)
## diesel = DERV (DUKES)

fuel.hhv <- as.Quantity(c(petrol = 47.1,
                          diesel = 45.7), "GJ t^-1")

## Emissions factors
## -----------------

## Source: DUKES 2012, Annex A, Fuel conversion factors ... 2010
## Units: Emissions of CO_2

fuel.co2 <- as.Quantity(c(gas = 0.185,
                          petrol = 0.240,
                          diesel = 0.252), "kg (kW h)_[energy]^-1")



