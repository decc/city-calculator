## Functions making up the City Calculator
## TODO: Make this an R package

library(plyr)

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
A 'Industrial and Commercial'
B 'Industrial and Commercial'
C 'Large Industrial Installations'
D 'Industrial and Commercial'
E 'Industrial and Commercial'
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
                            "Industrial and Commercial",
                            "Large Industrial Installations",
                            "LULUCF"),
                          ordered = TRUE)

## Select 2010. Restrict to those local authorities comprising the cities and to
## the high-level sectors

citiesco2 <- merge(cities, ukregco2)
citiesco2 <- merge(citiesco2, sector0)
citiesco2 <- citiesco2[citiesco2$yr == 2010,
                       c("city", "sector0", "sector_name", "CO2")]

## Cross-tab of total CO2 emissions
dcast(citiesco2, sector0 ~ city, margins = TRUE, sum)
format(dcast(citiesco2, sector0 ~ city, margins = TRUE, sum), big.mark = " ",
       format = "f")

