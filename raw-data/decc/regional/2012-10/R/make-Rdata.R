## Load the emissions and populations data sets as R data frames and save the
## corresponding R objects

setwd("../csv")

ukregco2 <- read.csv(file = "ukregco2.csv", na.strings = "#N/A")
ukregpop <- read.csv(file = "ukregpop.csv", na.string = "#N/A")
ukreggas <- read.csv(file = "ukreggas.csv", na.string = "#N/A")
ukreggas_excluded <- read.csv(file = "ukreggas_excluded.csv", na.string = "#N/A")
ukregelectricity <- read.csv(file = "ukregelectricity.csv", na.string = "#N/A")
ukregroadfuel <- read.csv(file = "ukregroadfuel.csv", na.string = "#N/A")
ukregotherfuels <- read.csv(file = "ukregotherfuels.csv", na.string = "#N/A")

setwd("../R")

save(ukregco2, ukregpop, file = "ukregco2.Rdata")
save(ukreggas, ukreggas_excluded, ukregelectricity,
     ukregroadfuel, ukregotherfuels, file = "ukregenergy.Rdata")


