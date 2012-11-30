## Functions making up the City Calculator
## TODO: Make this an R package(s)

library(plyr)
library(reshape2)

## Regional energy and emissions data
load("../data/regional/data/ukregenergy.Rdata")
load("../data/regional/data/ukregco2.Rdata")
load("../data/regional/data/gsslookup.Rdata")

# Energy conversion factors
source("../data/physical/data/conversion.R")

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

