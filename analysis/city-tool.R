## Functions making up the City Calculator
## TODO: Make this an R package

library(plyr)
library(reshape2)

## Regional emissions data
load("../data/emissions/data/ukreg.Rdata")

## Cities of interest
cities <- read.table(header = TRUE, text = "
city la_name
Manchester Bolton
Manchester Bury
Manchester Oldham
Manchester Rochdale
Manchester Stockport
Manchester Tameside
Manchester Trafford
Manchester Wigan
Manchester Manchester
Manchester Salford
Birmingham Birmingham
Leeds Leeds
Nottingham Nottingham
Newcastle 'Newcastle upon Tyne'
")

## High-level sectors
sector0 <- read.table(header = TRUE, text = "
sector_code sector0
A 'Ind. & Comm.'
B 'Ind. & Comm.'
C 'Large Ind.'
D 'Ind. & Comm.'
E 'Ind. & Comm.'
F 'Transport'
G 'Domestic'
H 'Domestic'
I 'Domestic'
J 'Transport'
K 'Transport'
L 'Transport'
M 'Transport'
N 'LULUCF'
")

sector0$sector0 <- factor(sector0$sector0,
                          c("Domestic", "Transport",
                            "Ind. & Comm.",
                            "Large Ind.",
                            "LULUCF"),
                          ordered = TRUE)

## Select 2010. Restrict to those local authorities comprising the cities and to
## the high-level sectors

cities.co2 <- merge(cities, ukregco2)
cities.co2 <- merge(cities.co2, sector0)
cities.co2 <- cities.co2[c("city", "la_name", "sector0", "sector_name", "yr", "CO2")]




