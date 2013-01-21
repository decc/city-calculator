### A City Calculator

library(siunits)
source("calculator.R") # Functions for building calculators
source("models/transport.R", chdir = TRUE)

## For testing purposes, build a simple transport model

## The following data is from the 2050 calculator, version 07/12/2012
## Sector XII.a | Domestic passenger transport | 2007 | Trajectory 1

population.uk <- 60973000 # people

distance_per_person <- as.Quantity(14104, "km") # per person per year

mode.share <- c(car = 0.84,
                bus = 0.06,
                train = 0.07,
                human = 0.03) # excludes air

occupancy <- c(car = 1.45,
               bus = 9.05,
               train = 0.324,
               human = 1.0) # train occupancies are per seat-km, not per
                            # vehicle-km

tech.share <- list(car = c(ICE = 0.99, PHEV = 0.01), # ADJUSTED to include PHEV
                                        # for testing
                   bus = c(ICE = 1.00),
                   train = c(DIESEL = 0.35, ELECTRIC = 0.65),
                   human = c(WALK = 0.81, BIKE = 0.19))

passenger_transport <- passenger_transport.model(N = population.uk,
                                                 d = distance_per_person,
                                                 mode.share,
                                                 occupancy,
                                                 tech.share)

flows <- passenger_transport() # Should be run_model(passenger.transport) !!

summarise_flows(flows)


