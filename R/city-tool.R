## Functions making up the City Calculator
## TODO: Make this an R package

library(plyr)
library(reshape2)

## Regional energy and emissions data
load("../data/regional/data/ukreg.Rdata")

## Cities of interest
city_codes <- read.table(header = TRUE, text = "
city geography_code geography_name
Manchester E08000001 Bolton
Manchester E08000002 Bury
Manchester E08000003 Manchester
Manchester E08000004 Oldham
Manchester E08000005 Rochdale
Manchester E08000006 Salford
Manchester E08000007 Stockport
Manchester E08000008 Tameside
Manchester E08000009 Trafford
Manchester E08000010 Wigan
Birmingham E08000025 Birmingham
Leeds E08000035 Leeds
Nottingham E06000018 Nottingham
Newcastle E08000021 'Newcastle upon Tyne'
")

## High-level emissions sectors
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




