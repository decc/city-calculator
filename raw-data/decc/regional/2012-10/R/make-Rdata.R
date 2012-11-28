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

## Redefine the sectors to be consistent across all fuels and CO2; convert
## energy to a common physical unit and add units to CO2 emissions (uses the
## `siunits` package).

fuels <- c("gas", "electricity", "petrol", "diesel", "coal",
           "petroleum", "manufactured solid fuels",
           "renewables & waste") 

sectors <- c("domestic", "commercial & industrial",
             "cars & motor-cycles", "buses", "LGVs", "HGVs", "rail",
             "agriculture", "LULUCF")

## A high-level aggregation for comparison with the total CO2 emissions 
sector0s <- c("domestic", "commercial & industrial", "road transport", "rail",
              "agriculture", "LULCF")

co2.sectors <- c(A = "Industry and Commercial Electricity",
                 B = "Industry and Commercial Gas",
                 C = "Large Industrial Installations",
                 D = "Industrial and Commercial Other Fuels",
                 E = "Agricultural Combustion",
                 F = "Railways",
                 G = "Domestic Electricity",
                 H = "Domestic Gas",
                 I = "Domestic 'Other Fuels'",
                 J = "Road Transport (A roads)",
                 K = "Road Transport (Motorways)",
                 L = "Road Transport (Minor Roads)",
                 M = "Road Transport Other",
                 N  = "LULUCF Net Emissions")

out.names <- c("yr", "lau", "geography_code", "sector", "fuel", "energy")

## Gas
## ---

gas <- transform(
  ukreggas,
  fuel = factor("gas", levels = fuels),
  sector = factor(
    c("domestic", "commercial & industrial")[match(
      sector,
      c("Domestic consumers", "Commercial and industrial consumers"))],
    levels = sectors),
  energy = as.Quantity(GWh, "(GW h)_[energy]"))[ , out.names]

## Electricity
## -----------

electricity <- transform(
  ukregelectricity,
  fuel = factor("electricity", levels = fuels),
  sector = factor(
    c("domestic", "commercial & industrial")[match(
      sector,
      c("Domestic consumers", "Commercial and industrial consumers"))],
    levels = sectors),
  energy = as.Quantity(GWh, "(GW h)_[energy]"))[ , out.names]

## Road fuels
## ----------

## CALORIFIC VALUES
##
## Source: DUKES 2012, Annex A, Estimated average calorific values 2011
## Note: The following are gross calorific values (higher heating value)
## Terminology:
## Here     DUKES
## ----     -----
## petrol = Motor spirit
## diesel = DERV

fuel.hhv <- as.Quantity(c(petrol = 47.1,
                          diesel = 45.7), "GJ t^-1")

roadfuel <- transform(
  ukregroadfuel,
  fuel = factor(
    c("diesel", "diesel", "diesel", "diesel", "petrol", "petrol", "petrol")[match(
      vehicle_type,
      c("Buses", "Diesel Cars", "Diesel LGV", "HGV", "Motor-cycles",
        "Petrol Cars", "Petrol LGV"))],
    levels = fuels),
  sector = factor(
    c("buses", "cars & motor-cycles", "LGVs", "HGVs", "cars & motor-cycles",
      "cars & motor-cycles", "LGVs")[match( 
        vehicle_type,
        c("Buses", "Diesel Cars", "Diesel LGV", "HGV", "Motor-cycles",
          "Petrol Cars", "Petrol LGV"))],
    levels = sectors),
  energy = as.Quantity(thousand.tonnes, "kt"))[ , out.names]

## Convert thousand tonnes of fuel to energy
hhv <- fuel.hhv[as.character(roadfuel$fuel)]
energy <- as.Quantity(roadfuel$energy * hhv, "(GW h)_[energy]")
names(energy) <- NULL
roadfuel$energy <- energy

## Other fuels

match(ukregroadfuel$vehicle_type, c("Buses", "Diesel Cars", "Diesel LGV", "HGV", "Motor-cycles", "Petrol Cars", "Petrol LGV"))
