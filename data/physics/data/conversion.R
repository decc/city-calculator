## Physical constants
## ------------------
##
## Includes energy conversion factors, calorific values, and emissions factors.

Source: DUKES 2012

energy.units = list(J = 1, Wh = 60 * 60)

## "Small-c" calorie. Source: International Steam Table calorie (1956)
## see, eg, http://en.wikiepedia.org/wiki/Calorie
##
energy.units = c(energy.units, cal = 4.1868 * energy.units$J, )

## Tonne of oil equivalent. Source: DUKES 2012, Annex A



## change_units(v, from, to)
## 
