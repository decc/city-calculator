# Load the emissions and populations data sest as R tables and save the corresponding R objects

ukregco2 <- read.csv(file = "ukregco2.csv", na.strings = "#N/A")
ukregpop <- read.csv(file = "ukregpop.csv", na.string = "#N/A")
ukreggas <- read.csv(file = "ukreggas.csv", na.string = "#N/A")
ukreggas_excluded <- read.csv(file = "ukreggas_excluded.csv", na.string = "#N/A")
ukregelectricity <- read.csv(file = "ukregelectricity.csv", na.string = "#N/A")
ukregroadfuel <- read.csv(file = "ukregroadfuel.csv", na.string = "#N/A")
ukregotherfuels <- read.csv(file = "ukregotherfuels.csv", na.string = "#N/A")

save(ukregco2, ukregpop, ukreggas, ukreggas_excluded, ukregelectricity,
     ukregroadfuel, ukregotherfuels, file = "ukreg.Rdata")


