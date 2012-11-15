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
## should be present in Quantity.Kinds
## Example: qk_expr <- c(energy = 1, mass = -1)
## Returns a 7-element vector of the powers of the basis vectors

as_vector <- function(qk_expr) {
    Reduce(`+`,                                       # Add up ...
           Map(`*`,                                   # the powers of ..
               qk_expr,                            
               lapply(Quantity.Kinds[names(qk_expr)],  # the basis dimensions
                      function(x) {`[[`(x, "vector")} ))) # (extracting the vectors).   
}

add_quantityKind <- function(name, definition) {

  if (name %in% names(Quantity.Kinds)) {
    stop("quantity kind [", name, "] already defined")
    }

  Quantity.Kinds[[name]] <<- list(definition = definition,
                                   vector = as_vector(definition))
}

get_unit_loc <- function(symbol) {}

add_unit0 <- function(qk, symbol, name, plural.name, coherent, multiple, series) {

  if (any(symbol %in% Units$basis$symbol)) {
    stop("unit '", symbol, "' is already a basis unit.")
  } else if (any(symbol %in% Units$coherent$symbol)) {
    stop("unit '", symbol, "' is already defined as a coherent unit.")
  } else if (any(symbol %in% Units$other$symbol)) {
    stop("unit '", symbol, "' is already defined.")
  }

  if (coherent) {
    Units$coherent <<- rbind(Units$coherent,
                             data.frame(qk = qk,
                                        symbol = symbol,
                                        name = name,
                                        plural.name = plural.name))
  } else {
    Units$other <<- rbind(Units$other,
                          data.frame(qk = qk,
                                     symbol = symbol,
                                     name = name,
                                     plural.name = plural.name,
                                     series = series,
                                     multiple = multiple))
  }
}



## Add units, including making up all the prefixed versions, calling add_unit0
## for each individual unit (and to check for duplicates).

add_unit <- function(qk, symbol, name, plural.name = "", coherent = FALSE, multiple
                     = 1.0, gen.prefixes = FALSE, true.basis = NA) {
  
  if (is.na(qk)) {
    stop("quantity kind must be given for unit ", symbol)
  }
  
  if (plural.name == "") plural.name <- paste(name, "s", sep = "")
  
  if (!gen.prefixes) {
    add_unit0(qk, symbol, name, plural.name, coherent, multiple, NA) 
  } else {
    
    ## Prefixed units are added one at a time so that add_unit0 can report
    ## sensible errors on duplicates. We don't expect new units to be added
    ## very frequently.

    if (is.na(true.basis)) {
      add_unit0(qk, symbol, name, plural.name, coherent, multiple, symbol)
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else if (true.basis == symbol) {
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else {
      add_unit0(qk, symbol, name, plural.name, FALSE, multiple, symbol)
      skip <- match(true.basis, paste(SI.Prefixes$prefix, symbol, sep = ""))
      prefix.range <- (1:length(SI.Prefixes$prefix))[-skip]
      skip.multiple <- SI.Prefixes$multiple[[skip]]
    }
    
    for (i in prefix.range) {
      add_unit0(qk,
                paste(SI.Prefixes$prefix[[i]], symbol, sep = ""),
                paste(SI.Prefixes$name[[i]], name, sep = ""),
                paste(SI.Prefixes$name[[i]], plural.name, sep = ""),
                FALSE, # These are not coherent derived units
                SI.Prefixes$multiple[[i]] / skip.multiple,
                symbol)
    }
  }
}


## Definitions of quantity kinds
## Should be package local in the end

Bases <- data.frame(
  symbol = c("L", "M", "T", "I", "Th", "N", "J"),
  name = c("length", "mass", "time", "electric current",
    "thermodynamic temperature", "amount of substance", "luminous intensity")) 

## Units in basis.units are the default for those dimensions whose
## definition is NULL.

Units <- list(
  basis = data.frame(
    symbol = c("m", "kg", "s", "A", "K", "mol", "cd"),
    name = c("metre", "kilogram", "second", "ampere", "kelvin", "mole",
      "candela"),
    plural.name = c("metres", "kilograms", "seconds", "amperes", "kelvins",
      "moles", "candelas")),
  coherent = data.frame(), # qk, symbol, name, plural.name
  other = data.frame()) # qk, symbol, name, plural.name, series, multiple

SI.Prefixes <- data.frame(name = c("yotta", "zetta", "exa", "peta", "tera",
                            "giga", "mega", "kilo", "hecto", "deca", "deci",
                            "centi", "milli", "micro", "nano", "pico", "femto",
                            "atto", "zepto", "yocto"),
                          multiple = c(1e24, 1e21, 1e18, 1e15, 1e12, 1e9, 1e6, 1e3,
                            1e2, 10, 0.1, 1e-2, 1e-3, 1e-6, 1e-9, 1e-12, 1e-15,
                            1e-18, 1e-21, 1e-24), 
                          prefix = c("Y", "Z", "E", "P", "T", "G", "M", "k", "h",
                            "da", "d", "c", "m", "µ", "n", "p", "f", "a", "z", "y"))


## TODO: Add new unit sets: coherent (J, N, ...) and prefixed (GJ, g, ...)
## Same format, but now each unit lists its known quantityKinds.
## And also need a "default units map quantityKinds -> Units$.

Quantity.Kinds <- list(
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
               mapply(format_unit, Units[[c("basis", "symbol")]],
                      si)),
        collapse = " ")
}


## Create a reasonably complete set of dimensions and units

## The SI prefixed versions of the base units.
add_unit("M", "g", "gram", coherent = TRUE, gen.prefixes = TRUE, true.basis = "kg")
add_unit("L", "m", "metre", coherent = TRUE, gen.prefixes = TRUE, true.basis = "m")
add_unit("T", "s", "second", coherent = TRUE, gen.prefixes = TRUE, true.basis = "s")
add_unit("I", "A", "ampere", coherent = TRUE, gen.prefixes = TRUE, true.basis = "A")
add_unit("Th", "K", "kelvin", coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "K")
add_unit("N", "mol", "mole", coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "mol") 
add_unit("J", "cd", "candela", coherent = TRUE, gen.prefixes = TRUE, true.basis
         = "cd")

## Some dimensions which don't have their own units in SI
add_quantityKind(name = "velocity",
                 definition = c(displacement = 1, time = -1)) # m/s
add_quantityKind(name = "acceleration",
                 definition = c(velocity = 1, time = -1)) # (m/s)/s
add_quantityKind(name = "area",
                 definition = c(displacement = 2)) # m^2

## Dimensions with their own units
add_quantityKind(name = "frequency",
                 definition = c(time = -1))
add_unit("frequency", "Hz", "hertz", "hertz", coherent = TRUE, gen.prefixes =
                 TRUE)

add_quantityKind(name = "angle",
                 definition = c(displacement = 1, position = -1)) # rad = m/m
add_quantityKind(name = "solid_angle",
                 definition = c(angle = 2)) # sr = rad^2
add_quantityKind(name = "momentum",
                 definition = c(mass = 1, velocity = 1)) # kg (m/s)
add_quantityKind(name = "force",
                 definition = c(mass = 1, acceleration = 1)) # N = kg (m/s)/s 
                                        # Should be (momentum = 1,
                                        # time = -1?)
add_quantityKind(name = "pressure",
                 definition = c(force = 1, area = -1)) # Pa = N/m^2
add_quantityKind(name = "energy",
                 definition = c(force = 1, displacement = 1)) # J = N m
add_quantityKind(name = "power",
                 definition = c(energy = 1, time = -1)) # W = J/s
add_quantityKind(name = "electric_charge",
                 definition = c(electric_current = 1, time = -1)) # C = A s
add_quantityKind(name = "voltage",
                 definition = c(energy = 1, electric_charge = -1)) # V = J / C.


