## Physical constants
## ==================
##
## Energy conversion factors, calorific values, and emissions factors.
##

## Energy conversion factors
## -------------------------

energy.units = list(J = 1, Wh = 60 * 60 * 1)

## "Small-c" calorie.
## Definition: International Steam Table calorie (1956)
## See, eg, http://en.wikipedia.org/wiki/Calorie
## This also appears to be the definition used by DUKES

energy.units = c(energy.units, list(cal = 4.1868 * energy.units$J))

## Tonne of oil equivalent.
## Definition: OECD/IEA definition, also the one used by DUKES

energy.units = c(energy.units, list(toe = 10e7 * energy.units$cal))

## SI prefixes

si.prefix = list(k = 1e3, M = 1e6, G = 1e9, T = 1e12, P = 1e15, E
  = 1e18, Z = 1e21, Y = 1e24)

## to_base_unit

to_base_unit <- function(q) {}
  


## change_units(v, from, to)
## 
