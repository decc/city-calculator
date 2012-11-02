## Physical constants
## ==================
##
## Energy conversion factors, calorific values, and emissions factors.
##

## Energy conversion factors
## -------------------------

## Base units

energy.unit = list(J = 1, Wh = 60 * 60 * 1)

## "Small-c" calorie.
## Definition: International Steam Table calorie (1956)
## See, eg, http://en.wikipedia.org/wiki/Calorie
## This also appears to be the definition used by DUKES

energy.unit = c(energy.unit, list(cal = 4.1868 * energy.unit$J))

## Tonne of oil equivalent.
## Definition: OECD/IEA definition, also the one used by DUKES

energy.unit = c(energy.unit, list(toe = 1e10 * energy.unit$cal))

## SI prefixes

si.prefix = list(k = 1e3, M = 1e6, G = 1e9, T = 1e12, P = 1e15, E
  = 1e18, Z = 1e21, Y = 1e24)

## Calorific values
## ----------------
##
## Source: DUKES 2012, Annex A, Estimated average calorific values 2011
## Note: The following are gross calorific values (higher heating value)
## Terminology:
## petrol = Motor spirit (DUKES)
## diesel = DERV (DUKES)

fuel.hhv <- list(petrol = 47.1, diesel = 45.7) # Units: GJ / tonne

## Emissions factors
## -----------------

## Source: DUKES 2012, Annex A, Fuel conversion factors ... 2010

fuel.co2 <- list(gas = 0.185, petrol = 0.240, diesel = 0.252) # kg CO2 / kWh



