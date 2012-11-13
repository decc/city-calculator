### Units - A physical quantity package for R

## Notes:
## - Ratio scales only -- no degrees Celsius to Kelvin
## - Currently uses SI as the underlying basis
## - Does not do conversions between coherent systems

## Data structures
## ---------------

units <- NULL # listof unit

## add.QuantityKind
##
## Adds a new quantity kind to the package-global list. Returns the new quantity
## kind. name should not be the name of an exisiting quantity kind and should be
## coercable to symbol. definition is a list consisting of pairs (quantity kind,
## power).

## qk_expr should be an integer vector giving powers of quantity kinds, which
## should be present in quantity.kinds
## Example: qk_expr <- c(energy = 1, mass = -1)
## Returns a 7-element vector of the powers of the basis vectors

get.si.basis <- function(qk) {
  qk$si.basis
}

compute.si.basis <- function(qk_expr) {
  Reduce(`+`,
         Map(`*`, qk_expr, lapply(quantity.kinds[names(qk_expr)], get.si.basis)))
}

add.QuantityKind <- function(name, definition) {
  if (name %in% names(quantity.kinds)) {
    stop("quantity kind [", name, "] already defined")
  }
  new.quantity.kinds <- c(quantity.kinds,
                          list(
                            list(definition = definition,
                                 si.basis =
                                 compute.si.basis(definition))))
  names(new.quantity.kinds) <- c(names(quantity.kinds), name)
  quantity.kinds <<- new.quantity.kinds
}


## Definitions of quantity kinds
## Should be package local in the end

si.bases <- list(symbol = c("L", "M", "T", "I", "Th", "N", "J"),
                 name = c("length", "mass", "time", "electric current",
                 "thermodynamic temperature", "amount of substance", "luminous intensity"))

quantity.kinds <- list(
  position = list(
    definition = NULL,
    si.basis = c(1,0,0,0,0,0,0)),
  displacement = list(
    definition = NULL,
    si.basis = c(1,0,0,0,0,0,0)),
  mass = list(
    definition = NULL,
    si.basis = c(0,1,0,0,0,0,0)),
  time = list(
    definition = NULL,
    si.basis = c(0,0,1,0,0,0,0)),
  electric_current = list(
    definition = NULL,
    si.basis = c(0,0,0,1,0,0,0)),
  temperature = list(
    definition = NULL,
    si.basis = c(0,0,0,0,1,0,0)),
  amount = list(
    definition = NULL,
    si.basis = c(0,0,0,0,0,1,0)),
  luminous_intensity = list(
    definition = NULL,
    si.basis = c(0,0,0,0,0,0,1)))
  

add.QuantityKind("velocity",
                 list(displacement = 1, time = -1)) # m/s

add.QuantityKind("acceleration",
                 list(velocity = 1, time = -1)) # (m/s)/s

add.QuantityKind("area",
                 list(displacement = 2)) # m^2

add.QuantityKind("frequency",
                 list(time = -1)) # Hz = s^-1 ????? Maybe frequency is
                                  # fundamental? s Hz is ... what? N??

add.QuantityKind("angle",
                 list(displacement = 1, position = -1)) # rad = m/m

add.QuantityKind("solid_angle",
                 list(angle = 2)) # sr = rad^2

add.QuantityKind("momentum",
                 list(mass = 1, velocity = 1)) # kg (m/s)

add.QuantityKind("force",
                 list(mass = 1, acceleration = 1)) # N = kg (m/s)/s 
                                        # Should be (momentum = 1,
                                        # time = -1?)

add.QuantityKind("pressure",
                 list(force = 1, area = -1)) # Pa = N/m^2

add.QuantityKind("energy",
                 list(force = 1, displacement = 1)) # J = N m

add.QuantityKind("power",
                 list(energy = 1, time = -1)) # W = J/s

add.QuantityKind("electric_charge",
                 list(electric_current = 1, time = -1)) # C = A s

add.QuantityKind("voltage",
                 list(energy = 1, electric_charge = -1)) # V = J / C.


