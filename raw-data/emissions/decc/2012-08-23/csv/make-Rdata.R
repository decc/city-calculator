# Load the emissions and populations data sest as R tables and save the corresponding R objects

ukregco2 <- read.csv(file = "emissions.csv", na.strings = "#N/A")

ukregpop <- read.csv(file = "population.csv", na.string = "#N/A")

save(ukregco2, ukregpop, file = "ukreg.Rdata")


