### Units - A physical quantity package for R

## Notes:
##
## - Ratio scales only -- no degrees Celsius to Kelvin
## - Currently uses SI as the underlying basis
## - Does not do conversions between coherent systems


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

as_vector <- function(qk_expr) {
    Reduce(`+`,                                       # Add up ...
           Map(`*`,                                   # the powers of ..
               qk_expr,                            
               lapply(quantityKinds[names(qk_expr)],  # the basis dimensions
                      function(x) {`[[`(x, "vector")} ))) # (extracting the si vectors).   
}

add_QuantityKind <- function(name, definition) {

    if (name %in% names(quantityKinds)) {
        stop("quantity kind [", name, "] already defined")
    }

    quantityKinds[[name]] <<- list(definition = definition,
                                   vector = as_vector(definition))
}


## Definitions of quantity kinds
## Should be package local in the end

Bases <- data.frame(
    symbol = c("L", "M", "T", "I", "Th", "N", "J"),
    name = c("length", "mass", "time", "electric current",
        "thermodynamic temperature", "amount of substance", "luminous intensity")) 

## unitSets: listof groups of units
## at least one unit set, named "_basis" must be present
## other derived units may be added. The list _basis must be of the same length
## and in the same order as Bases.

## Units in the _basis unit set are the default for those dimensions whose
## definition is NULL.

unitSets <- list(
    "_basis" = list(
        symbol = c("m", "kg", "s", "A", "K", "mol", "cd"),
        name = c("metre", "kilogram", "second", "ampere", "kelvin", "mole",
            "candela"),
        plural.name = c("metres", "kilograms", "seconds", "amperes", "kelvins",
            "moles", "candelas")))

## TODO: Add new unit sets: coherent (J, N, ...) and prefixed (GJ, g, ...)
## Same format, but now each unit lists its known quantityKinds.
## And also need a "default units map quantityKinds -> units$.

quantityKinds <- list(
    position = list(
        definition = NULL,
        vector = c(1,0,0,0,0,0,0)),
    displacement = list(
        definition = NULL,
        vector = c(1,0,0,0,0,0,0)),
    mass = list(
        definition = NULL,
        vector = c(0,1,0,0,0,0,0)),
    time = list(
        definition = NULL,
        vector = c(0,0,1,0,0,0,0)),
    electric_current = list(
        definition = NULL,
        vector = c(0,0,0,1,0,0,0)),
    temperature = list(
        definition = NULL,
        vector = c(0,0,0,0,1,0,0)),
    amount = list(
        definition = NULL,
        vector = c(0,0,0,0,0,1,0)),
    luminous_intensity = list(
        definition = NULL,
        vector = c(0,0,0,0,0,0,1)))


## format_si_vector
## Convert a vector of powers of basis units to a string
## eg, format_si_vector(c(1,0,-1,0,0,0,0)) -> "m s^-1"

format_unit <- function(symbol, power) {
    if (power == 0) {
        NULL
    } else if (power == 1) {
        symbol
    } else {
        paste(symbol, "^", power, sep = "")
    }
}

format_vector <- function(si) {
    paste(
        unlist( # Drops any NULLs returned by format_si_unit 
               mapply(format_unit, unitSets[[c("_basis", "symbol")]],
                      si)),
        collapse = " ")
}
    
    


add_QuantityKind(name = "velocity",
                 definition = c(displacement = 1, time = -1)) # m/s
add_QuantityKind(name = "acceleration",
                 definition = c(velocity = 1, time = -1)) # (m/s)/s
add_QuantityKind(name = "area",
                 definition = c(displacement = 2)) # m^2
add_QuantityKind(name = "frequency",
                 definition = c(time = -1)) # Hz = s^-1 ????? Maybe frequency is
                                        # fundamental? s Hz is ... what? N??
add_QuantityKind(name = "angle",
                 definition = c(displacement = 1, position = -1)) # rad = m/m
add_QuantityKind(name = "solid_angle",
                 definition = c(angle = 2)) # sr = rad^2
add_QuantityKind(name = "momentum",
                 definition = c(mass = 1, velocity = 1)) # kg (m/s)
add_QuantityKind(name = "force",
                 definition = c(mass = 1, acceleration = 1)) # N = kg (m/s)/s 
                                        # Should be (momentum = 1,
                                        # time = -1?)
add_QuantityKind(name = "pressure",
                 definition = c(force = 1, area = -1)) # Pa = N/m^2
add_QuantityKind(name = "energy",
                 definition = c(force = 1, displacement = 1)) # J = N m
add_QuantityKind(name = "power",
                 definition = c(energy = 1, time = -1)) # W = J/s
add_QuantityKind(name = "electric_charge",
                 defintion = c(electric_current = 1, time = -1)) # C = A s
add_QuantityKind(name = "voltage",
                 definition = c(energy = 1, electric_charge = -1)) # V = J / C.


