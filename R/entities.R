### Entities in the Calculator

## Labels
## ======

## A label is a character vector
## merge.labels: Returns the set-theoretic union of its arguments

merge.labels <- function(l1, l2) {
  unique(c(l1,l2))
}

## Entities
## ========

## Entities are:
## - Fuel types (abbreviated simply as "fuels")
## - Sectors

## The central problem this set of definitions attempts to solve is that
## entities may come in hierarchies. The present approach represents an entitiy
## as a character vector. The elements of the character vector are are the names
## of the entities, including all names in all levels of the hierarchy. So, for
## example, "petrol" may be represented as "

## The character vector lists the entity names from most specific to least specific.

## Fuel types
## ----------

## Flows are primarily characterised by their fuel type. Fuel types are arranged
## in a strict hierarchy. The meaning of the hierarchy is as follows: (1) Any
## activity which requires a fuel of type A is "allowed" to consume any fuel
## that is a subtype of A; (2) Any activity which produces a fuel of type B can
## be "considered" to have produced a fuel of a supertype of B.
##
## Fuel types are fixed once and for all. Every flow must have one and only one fuel
## type

## fuel
##  ~ real
##     ~ hydrocarbon
##        ~ liquid hydrocarbon
##            ~ petrol 
##            ~ diesel
##  ~ nominal
##      ~ primary supply
##      ~ final demand

FuelTypes <- c("gas", "electricity", "petrol", "diesel", "coal", "petroleum",
               "manufactured solid fuels", "renewables & waste",
               "primary supply", "final demand", "real", "nominal")

check.fueltype <- function(atomic_fuel) {
  if (!(atomic_fuel %in% FuelTypes))
    stop(atomic_fuel, " is not a recognised fuel type", call. = FALSE)
  return(atomic_fuel)
}

fueltype <- function(s) {
  switch(s,
         "real" = c("real", "fuel"),
         "nominal" = c("nominal", "fuel"),
         "primary supply" = c("primary supply", fueltype("nominal")),
         "final demand" = c("final demand", fueltype("nominal")),
         "electricity" = c("electricity", fueltype("real")),
         "hydrocarbon" = c("hydrocarbon", fueltype("real")),
         "liquid hydrocarbon" = c("liquid hydrocarbon", fueltype("hydrocarbon")),
         "petrol" = c("petrol", fueltype("liquid hydrocarbon")),
         "diesel" = c("diesel", fueltype("liquid hydrocarbon")),
         "person" = c("person", fueltype("real")))
}


## Sectors
## -------

## Sectors describe something about the activity that produced the flow. Unlike
## fuel types, sectors do not form a strict hierarchy. For example, "van"
## might appear under both "passenger transport" and "freight
## transport". Indeed, "car" could might appear under both "transport" and
## "agriculture" (if transport on farms were classified under
## transport). Sectors are useful in presenting the results of the model.

## Here, we represent a sector as a list of labels, without any reference to a
## structure. It is up to the user to arrange these in a useful hierarchy. 

## sector
##  ~ transport
##      +WHAT
##        ~ passenger
##        ~ freight
##      +MODE
##        ~ road transport
##      +VEHICLE
##        ~ car transport


sector <- function(s) {
  switch(s,
         "transport" = c("transport", "sector"),
         "car" = c("car", sector("transport")),
         "road" = c("road", sector("transport")),
         "passenger" = c("passenger", sector("transport")))
}
