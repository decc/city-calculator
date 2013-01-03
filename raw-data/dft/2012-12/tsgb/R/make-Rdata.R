## Make R objects out of TSGB data

library(reshape2)
library(siunits)

## Table ENV0101 to flat table format

fuel_wide <- read.csv(file = "../csv/env0101.csv", header = TRUE)
fuel <- melt(fuel_wide,
             id.vars = c("mode", "fuel", "vehicle"), variable.name = "yr", value.name = "consumption")
fuel$yr <- as.numeric(substr(fuel$yr, 2, 5))
fuel$consumption = as.Quantity(fuel$consumption, "Mt")
save(fuel, file = "env0101.Rdata")

## Table TRA0201 to flat table format.
  
trans_wide <- read.csv(file = "../csv/tra0201.csv", header = TRUE)
trans <- melt(trans_wide, id.vars = c("Year"), variable.name = "vehicle", value.name = "distance")
trans$distance = as.Quantity(trans$distance, "Tm") # Terametres!
save(trans, file = "tra0201.Rdata")
