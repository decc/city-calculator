### Functions for building "Calculator"-type models
### Author: James Geddes
###         james.geddes@decc.gsi.gov.uk
###
### Very preliminary version
### TODO:
### - decide whether labels are text or symbols or values
###

source("entities.R")
  
## Flows
## =====

## Flow: Create a Flow given an energy value and a set of labels
## value -- a Quantity with signature "energy"
## direction -- either "in" or "out"
## fuel -- a vector of fuel types
## sector -- a vector of sector names

Flow <- function(value, direction = NA, fuel = character(), sector = character()) {
  list(value,
       direction = direction,
       fuel = fuel,
       sector = sector)
}

flow.direction <- function(f) {
  f$direction
}

flow.fuel <- function(f) {
  f$fuel
}

flow.sector <- function(f) {
  f$sector
}

flow.value <- function(f) {
  f[[1]]
}

## flow.scalar_mult : numeric Flow -> Flow
## Multiply the value of a flow by a numeric
flow.scalar_mult <- function(v, flow) {
  list(v * flow[[1]], flow[-1])
}

## Flowset
## =======

## A list of flows such that the sum of all flows is zero.

## Activities
## ==========

## Activity: Create an Activity given a function that returns a list of flows
make_Activity <- function(act) {
  structure(act, class = "Activity")
}

is.Activity <- function(x) {
  inherits(x, "Activity")
}

## constant_Activity: Make an activity which produces constant flows
make_constant_Activity <- function(flows) {
  make_Activity(function() {flows})
}

## act.scalar_mult : numeric Activity -> Activity
## Multiply an Activity by a numeric
## v: a numeric of length 1
## act: an Activity
act.scalar_mult <- function(v, act) {
  new.act <- function(...) {
    lapply(do.call(act, as.list(match.call()[-1])),
           function(flow) { flow.scalar_mult(v, flow) })}
  formals(new.act) <- formals(act)
  new.act
}

mm <- function(f) {
  g <- function(...) {
    2 * do.call(f, as.list(match.call()[-1]))
  }
  formals(g) <- formals(f)
  g  
}




## act.add Activity ... -> Activity
## If there is one argument it must be a   
## Add one or more Activities, which must all have the same formal arguments
act.add <- function(...) {
  
}



## label.activity
## Given an activity and a list of sectors, return an activity which produces
## the same set of flows with the sectors added to the flow labels if they don't
## already exist.
label <- function(act, sector = NULL) {
  function(...) {
    label.flows(act(...), sector = sector)
  }
}

## label.flow
## Given a flow and a set of labels, append the labels to the flow.
## For direction, a value of NA does not change the present value. 
label.flow <- function(f, direction = NA, fuel = NULL, sector = NULL) {
  Flow(flow.value(f),
       direction = if (is.na(direction)) flow.direction(f) else direction,
       fuel = merge.labels(flow.fuel(f), fuel),
       sector = merge.labels(flow.sector(f), sector))      
}

## label.flows
## Label a list of flows
label.flows <- function(fs, ...) {
  lapply(fs, function(x) {
    label.flow(x, ...)
  })
}
         

  
  

