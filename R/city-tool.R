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

## Emissions factors (includes direct emissions only)
## --------------------------------------------------
## Source: DUKES 2012, Annex A: "Fuel conversion factors for converting fossil
## fuels to carbon dioxide, 2010"
## Units: kg CO_2 / kWh

fuel.co2 <- as.Quantity(c(   gas = 0.185, # Natural gas
                          petrol = 0.240, 
                          diesel = 0.252,
                            coal = 0.285, # Industrial coal
                       petroleum = 0.267, # Fuel oil
      'manufactured solid fuels' = 0.369, # Coking coal
            'renewables & waste' = 0.000),
                        "kg (kW h)_[energy]^-1")

## Source: This is the figure used to impute the emissions from regional
## electricity consumption in the regional emissions dataset 2009. It is found
## in AEA's technical report for that dataset,
## http://www.decc.gov.uk/assets/decc/11/stats/climate-change/6225-technical-report-local-and-regional-carbon-dioxid.pdf

electricity.co2 <- as.Quantity(500, "t (GW h)_[energy]^-1")

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

