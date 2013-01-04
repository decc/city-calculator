### DUKES 2012
### Annex A.1
### Estimated average calorific values of fuels 2011
### Gross calorific values
### Selected fuels

library(siunits)

## Names
## 
## DUKES                                   This file
## -----                                   ---------
## Butane and propane (LPG)                lpg
## Motor spirit                            petrol
## DERV                                    diesel
## Aviation spirit and wide cut gasoline   aviation.spirit
## Aviation turbine fuel                   aviation.turbine.fuel
## Gas/diesel oil                          gas.oil
## Fuel oil                                fuel.oil

dukes.annex.A.1 <- as.Quantity(c(                  lpg = 49.3,
                                                petrol = 47.1,
                                                diesel = 45.7,
                                       aviation.spirit = 47.4,
                                 aviation.turbine.fuel = 46.2,
                                               gas.oil = 45.4,
                                              fuel.oil = 43.3),
                               "GJ t^-1")

