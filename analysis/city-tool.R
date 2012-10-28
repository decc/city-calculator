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
C 'Ind. & Comm.'
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
                            "LULUCF"),
                          ordered = TRUE)




