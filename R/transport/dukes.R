## DIGEST OF UK ENERGY STATISTICS 2012
## (selected data)
##
## Source: DUKES 2012, Printed version
## Published by TSO, www.tsoshop.co.uk
## ISBN 9780115155284
## See DECC's website for electronic editions which may have been revised from
## the figures reporduced here.

## TODO: Use Barons

library(siunits)

## Table 1: Aggregate energy balance (gross calorific values)

dukes_table_1_final_consumption <- read.table(header = TRUE, text = "
yr   dukes_sector           dukes_fuel                energy
2010 Air                   'Petroleum products' 12288 
2011 Air                   'Petroleum products' 12802 
2010 Rail                   Coal                14
2011 Rail                   Coal                11
2010 Rail                  'Petroleum products' 659
2011 Rail                  'Petroleum products' 652
2010 Rail                   Electricity         349
2011 Rail                   Electricity         349
2010 Road                  'Petroleum products' 39159
2011 Road                  'Petroleum products' 38646
2010 Road                  'Bioenergy & waste'  1214
2011 Road                  'Bioenergy & waste'  1128
2010 Road                   Electricity         2
2011 Road                   Electricity         2
2010 'National navigation' 'Petroleum products' 1469
2011 'National navigation' 'Petroleum products' 1597"
)

dukes_table_1_final_consumption$energy <- as.Quantity(
  dukes_table_1_final_consumption$energy, "ktoe")

dukes_consumption <- function(yr, dukes_sector, dukes_fuel) {
  dukes_table_1_final_consumption$energy[dukes_table_1_final_consumption$yr == yr &
                                         dukes_table_1_final_consumption$dukes_sector == dukes_sector &
                                         dukes_table_1_final_consumption$dukes_fuel == dukes_fuel]
}

