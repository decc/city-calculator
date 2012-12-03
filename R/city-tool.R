## Functions making up the City Calculator
## TODO: Make this an R package(s)

library(plyr)
library(reshape2)
library(siunits)

## Regional energy and emissions data
## ----------------------------------
load("../data/regional/data/ukregenergy.Rdata")
load("../data/regional/data/ukregco2.Rdata")
load("../data/regional/data/gsslookup.Rdata")

## Emissions factors
## -----------------
## Source: DUKES 2012, Annex A, "Fuel conversion factors [...] 2010"
## Units: Emissions of CO_2

fuel.co2 <- as.Quantity(c(   gas = 0.185,
                          petrol = 0.240,
                          diesel = 0.252), "kg (kW h)_[energy]^-1")

## Fuels and Sectors
## -----------------

Fuels <- c("gas", "electricity", "petrol", "diesel", "coal", "petroleum",
           "manufactured solid fuels", "renewables & waste")

Sectors <- c("domestic", "commercial & industrial", "transport", "all sources",
             "LULUCF") # labelled `sector0` in the datasets


## Cities of interest
## ------------------

city_codes <- read.table(header = TRUE, text = "
city       geography_code geography_name
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
Leeds      E08000035 Leeds
Nottingham E06000018 Nottingham
Newcastle  E08000021 'Newcastle upon Tyne'
")

##

## Extract that part of a data frame containing city data
## Assumes: data is a data frame containing a column `geography_code`
## Returns: those rows for which `geography_code` is one of the local
## authorities in the city.

extract_city <- function(city, data) {
  data[data$geography_code %in%
       city_codes$geography_code[city_codes$city == city], ]
}
