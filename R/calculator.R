## Functions for building "Calculator"-type models
## 
## Author: James Geddes
##         james.geddes@decc.gsi.gov.uk

## Entities
## ========

FuelTypes <- c("gas", "electricity", "petrol", "diesel", "coal", "petroleum",
               "manufactured solid fuels", "renewables & waste")

fueltype <- function(fuel) {
  if (!(fuel %in% FuelTypes)) stop(fuel, " is not a recognised fuel type". call. = FALSE)
  return fuel
}


