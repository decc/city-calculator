## Make R objects out of TSGB data

library(reshape2)
library(siunits)

## Table ENV0101 to flat table format

env.wide <- read.csv(file = "../csv/env0101.csv", header = TRUE)
tsgb.env0101 <- melt(env.wide,
                     id.vars = c("mode", "fuel", "vehicle"),
                     variable.name = "yr", value.name = "consumption")
tsgb.env0101$yr <- as.numeric(substr(tsgb.env0101$yr, 2, 5))
tsgb.env0101$consumption = as.Quantity(tsgb.env0101$consumption, "Mt")
save(tsgb.env0101, file = "env0101.RData")

## Table TRA0201 to flat table format.

dist.wide <- read.csv(file = "../csv/tra0201.csv", header = TRUE, check.names = FALSE)
tsgb.tra0201 <- melt(dist.wide,
                     id.vars = c("Year"),
                     variable.name = "vehicle", value.name = "distance")
tsgb.tra0201$distance = as.Quantity(tsgb.tra0201$distance, "Tm") # Terametres!
save(tsgb.tra0201, file = "tra0201.RData")
