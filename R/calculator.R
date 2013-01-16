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

flow.direction <- function(f) { f$direction } # Either "in" or "out"
 
flow.fuel <- function(f) { f$fuel } # A character vector of atomic fuels

flow.sector <- function(f) { f$sector }

flow.value <- function(f) { f[[1L]] }

## flow.scalar_mult : numeric Flow -> Flow
## Multiply the value of a flow by a numeric
flow.scalar_multiply <- function(v, flow) {
  c(list(v * flow[[1L]]), flow[-1L])
}

## Flowlist
## ========

## A list of flows such that the sum of all flows is zero.

## summarise.flows flowlist -> summary
##
## Presently only summarises by fueltype and direction.
## TODO: Write function for pretty printing summary-type things
## TODO: Add an optional argument to take a fueltype hierarchy.

summarise.flows <- function(flowlist) {
  ## Create hierarchical list of used fuels
  fueltree <- entities_to_tree(lapply(flowlist, flow.fuel))
  

}


## fuels: list-of character vector
## A `tree` is either:
## - list()                  Empty tree
## - list("name")            Single node
## - list("name", tree, ...) Node with subnodes
##
## Notes: Trees are rooted. (tree, ...) is not a tree
##        list("name")[-1] -> list()
## TODO: Make a closure

entities_to_tree <- function(entities) {
  is.empty <- function(ll) { 
    isTRUE(length(ll) == 0L)
  }

  ## branch: character()
  insert_branch <- function(tree, branch) {
    ## If `branch` is empty, return `tree`. Else
    ## Either `tree` ... 
    ## - is empty, in which case
    ##   -> return (branch[[1]], insert_branch(list(), branch[-1])) 
    ## - has a root node which is not equal to branch[[1]], in which case
    ##   -> return FALSE (this tree has become inconsistent)
    ## - has a root node which is equal to branch[[1]], in which case,
    ## 
    if (is.empty(tree)) {
      list(this)
    } else if () {
      
    }  
    
  }
  
  add_to_tree <- function(tree, entity_list) {
    if (is.empty(entity_list)) {
      tree
    } else {
      add_to_tree(insert_branch(tree, rest[[1]]), rest[-1])
    }
  }
    
  add_to_tree(tree = list(), entities) 
}

  


## Activities
## ==========

## Activity: Create an Activity given a function that returns a list of flows
Activity <- function(act) {
  structure(act, class = "Activity")
}

is.Activity <- function(x) {
  inherits(x, "Activity")
}

## constant_Activity: Make an activity which produces constant flows
make_constant_Activity <- function(flows) {
  Activity(function() {flows})
}

## activity.scalar_mult : numeric Activity -> Activity
## Multiply an Activity by a numeric
## v: a numeric of length 1
## act: an Activity
activity.scalar_multiply <- function(v, act) {
  new.act <- function(...) {
    lapply(do.call(act, as.list(match.call()[-1L])),
           function(flow) {
             flow.scalar_multiply(v, flow)
           })
  }
  formals(new.act) <- formals(act)
  Activity(new.act)
}

## activity.add Activity ... -> Activity
##
## Add one or more Activities, which must all have the same formal arguments
## (which is not checked). The arguments of the return activity will the the
## formal arguments of the first argument to add.activity. If there is one argument
## it must be a list of Activities
activity.add <- function(...) {
  activities <- list(...)
  if (!length(activities)) {
    return(Activity(function() { list() }))
  }
  if (length(activities) == 1L && is.list(activities[[1L]])) {
    activities <- activities[[1L]]
  }
  new.act <- function(...) {
    matched_args <- match.call()[-1L]
    call_with_matched_args <- function(f) {
      do.call(f, as.list(matched_args))
    }
    unlist(lapply(activities, call_with_matched_args), recursive = FALSE)
  }
  formals(new.act) <- formals(activities[[1L]])
  Activity(new.act)
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
         

  
  

