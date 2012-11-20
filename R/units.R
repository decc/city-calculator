### Units - A physical quantity package for R

## Notes:
##
## - Ratio scales only -- no degrees Celsius to Kelvin
## - Uses SI as the underlying basis

indef.article <- function(str) {
  if (substr(str, 1, 1) %in% c("a", "A", "e", "E", "i", "I", "o", "O", "u",
                               "U")) {
    "an"
  } else {
    "a" }
}
    
## atomic_unit := one of a list of named units (kg, J, N, Gt, mHz, ...)

is.basis_unit <- function(au) {
  au %in% Units$symbol[Units$type == "basis"]
}

is.coherent_unit <- function(au) {
  au %in% Units$symbol[Units$type == "coherent"] | is.basis_unit(au)
}

is.atomic_unit <- function(au) {
  au %in% Units$symbol
}

atomic_unit.type <- function(au) {
  Units$type[Units$symbol == au]
}

## unit := vector-of (atomic_unit = power) such that no atomic_unit occurs twice and no
## power is zero

is.unit <- function(u) {
  (is.numeric(u)
   && all(is.atomic_unit(names(u))) 
   && !anyDuplicated(names(u))
   && !any(u == 0))
}

## unit.dimensions : unit -> dimension
## Extract the dimension of a unit
## Not vectorised

unit.dimension <- function(unit) {
  Units$dimension[Units$symbol == unit]
}

## A unit is a compatible unit for a dimension just in case the reduction of
## the dimensions of the unit to basis dimensions is the same as the reduction
## of the dimensions of the, er  ... dimension.

to_basis <- function(dim_expr) {
  Reduce(`+`,                                           # Add up ...
         Map(`*`,                                       # the powers of ..
             dim_expr,                            
             lapply(Dimensions[names(dim_expr)],     # the basis dimensions
                    function(x) {`[[`(x, "vector")} ))) # (extracting the vectors).   
}

## unit: vector-of (atomic_unit, power)
is.compatible_unit <- function(unit, dimension) {
  unit.dimensions <- unit
  names(unit.dimensions) <- Units$dimension[match(names(unit), Units$symbol)]
  
  identical(to_basis(unit.dimensions),
            Dimensions[[dimension]]$vector)
}

## 
## atomic_measure := pair (unit, dimension) such that unit is a compatible unit for dimension
## 
## measure := list-of (atomic_measure, power) such that no power is zero
##                

is.dimension <- function(dimension) {
  !is.null(Dimensions[[dimension]])
}

is.atomic_measure <- function(am) {
  (is.unit(am[[1]]) 
   && is.dimension(am[[2]])
   && is.compatible_unit(am[[1]], am[[2]]))
}

is.measure <- function(m) {
  (is.list(m)
   && all(vapply(m, is.measure_part, TRUE)))
}

is.measure_part <- function(mp) {
  (is.list(mp)
   && length(mp) == 2L
   && is.atomic_measure(mp[[1]])
   && !identical(mp[[2]], 0))
}



## Example atomic measures:
## N_[force]
## kg_[mass]
## (kg m s^-2)_[force]
## (m s^-1)_[velocity]
## (N m)_[energy]
##
## Example non-atomic measures:
## kg_[mass] (m s^-1)_[velocity]
## N_[force] m_[displacement]
## 
## Things that are not measures:
## * (N_[force] m_[displacement])_[energy] 
## * (N_[force] m_[position])_[torque] 

## Simplify: convert x-x an omitted symbol
## Reduce: convert to basis thingies

## Quantity: make a numeric object of class Quantity
## measure: either a measure, or a unit, or a string representing a unit

## TODO: Allow measure to be a string, eg, "kg m^2 s^-2"

Quantity <- function(vec, measure, dimension = NULL) {
  if (class(vec) == "Quantity") warn("'vec' is already a Quantity")
  if (!is.numeric(vec)) stop("'vec' must be numeric")
  if (!is.measure(measure)) stop("'measure' is not a valid measure")
  
  class(vec) <- c("Quantity", "numeric")
  attr(vec, "measure") <- measure
  vec
}

## string_to_measure: return a measure given a character string of the form "kg
## m^2 s^-2"

string_to_measure <- function(str) {
}

to_SI.Quantity <- function(q) {
  ## make units SI base units
}

## TODO: Need to capture the numeric-relevant options to 'print'

print.Quantity <- function(vec, verbose = FALSE, ...) {
  cat("Units:", format_measure(attr(vec, "measure"), verbose), "\n")
  print(as.numeric(vec), ...)
  invisible(vec)
}

format_measure <- function(measure, verbose = FALSE) {
  parens <- length(measure) > 1L
  paste(lapply(measure, format_measure_part, verbose, parens = parens), collapse = " ")
}

format_measure_part <- function(mp, verbose, parens = FALSE) {
  if (mp[[2]] == 1) {
    if (parens) {
      paste("(", format_atomic_measure(mp[[1]], verbose), ")", sep = "")
    } else {
      format_atomic_measure(mp[[1]], verbose)
    }
  } else {
    paste("(", format_atomic_measure(mp[[1]], verbose), ")^", mp[[2]], sep = "")
  }
}

format_atomic_measure <- function(am, verbose) {
  ff <- format_unit(am[[1]])
  if (!verbose) {
    ff 
  } else {
    paste("[", ff, "]_", am[[2]], sep = "")
  }
}

## format_unit: unit -> "kg m^2 s^-2" (eg)
format_unit <- function(unit) {
  paste(
    mapply(format_unit_part, names(unit), unit), collapse = " ")
}

## format_unit_part: -> m^2 (eg)
format_unit_part <- function(symbol, power) {
  if (power == 1) {
    symbol
  } else {
    paste(symbol, "^", power, sep = "")
  }
}


## format_vector <- function(vector) {
##    paste(
##        unlist( # Drops any NULLs returned by format_unit 
##               mapply(format_unit, Units[[c("basis", "symbol")]],
##                      vector)),
##        collapse = " ")
## }


q_to_unit <- function(vec, new.unit) {}
q_add <- function(q1, q2) {} # Can only add q's with identical measures
q_mult <- function(q1, q2, kind = NULL) {}



add_dimension <- function(name, definition) {
  if (name %in% names(Dimensions)) {
    stop("dimension [", name, "] already defined")
  }
  Dimensions[[name]] <<- list(definition = definition,
                                  vector = as_vector(definition))
}


add_unit0 <- function(dimension, symbol, name, plural.name, type, multiple, series) {

  if (any(is.atomic_unit(symbol))) {
    type <- atomic_unit.type(symbol)
    article <- indef.article(type) 
    stop("unit '", symbol, "' is already defined (as", article, atomic_unit.type(symbol), "unit)")
  }
  
  Units <<- rbind(Units,
                  data.frame(symbol = symbol,
                             dimension = dimension,
                             name = name,
                             plural.name = plural.name,
                             type = type,
                             multiple = multiple,
                             series = series))
}

## Add units, including making up all the prefixed versions, calling add_unit0
## for each individual unit (and to check for duplicates).
## TODO: Vectorise

add_unit <- function(dimension, symbol, name, plural.name = "", is.coherent = FALSE, multiple
                     = 1.0, gen.prefixes = FALSE, true.basis = NA, series = NA) {
  
  if (plural.name == "") plural.name <- paste(name, "s", sep = "")
  type <- if (is.coherent) "coherent" else "other"
  
  if (!gen.prefixes) {
    add_unit0(dimension, symbol, name, plural.name, type, multiple, series) 
  } else {
    
    ## Prefixed units are added one at a time so that add_unit0 can report
    ## sensible errors on duplicates. We don't expect new units to be added
    ## very frequently.

    if (is.na(true.basis)) {
      add_unit0(dimension, symbol, name, plural.name, type, multiple, series)
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else if (true.basis == symbol) {
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else {
      add_unit0(dimension, symbol, name, plural.name, "other", multiple, series)
      skip <- match(true.basis, paste(SI.Prefixes$prefix, symbol, sep = ""))
      prefix.range <- (1:length(SI.Prefixes$prefix))[-skip]
      skip.multiple <- SI.Prefixes$multiple[[skip]]
    }
    
    for (i in prefix.range) {
      add_unit0(dimension,
                paste(SI.Prefixes$prefix[[i]], symbol, sep = ""),
                paste(SI.Prefixes$name[[i]], name, sep = ""),
                paste(SI.Prefixes$name[[i]], plural.name, sep = ""),
                "other", # These are not coherent, derived units
                SI.Prefixes$multiple[[i]] / skip.multiple,
                series)
    }
  }
}


## Definitions of dimensions
## Should be package local in the end

Basis.Dimensions <- data.frame(
  symbol = c("L", "M", "T", "I", "Th", "N", "J"),
  name = c("length", "mass", "time", "electric current",
    "thermodynamic temperature", "amount of substance", "luminous intensity"),
  dimension = c("length", "mass", "time", "electric_current",
    "temperature", "amount", "luminous_intensity"),
  stringsAsFactors = FALSE) 

## Units in basis.units are the default for those dimensions whose
## definition is NULL. The order should be precisely the same as the order in
## Basis.Dimensions

Units <- data.frame(
  symbol = c("m", "kg", "s", "A", "K", "mol", "cd"),
  dimension = c("length", "mass", "time", "electric_current",
    "temperature", "amount", "luminous_intensity"), 
  name = c("metre", "kilogram", "second", "ampere", "kelvin", "mole",
    "candela"),
  plural.name = c("metres", "kilograms", "seconds", "amperes", "kelvins",
    "moles", "candelas"),
  type = "basis",
  multiple = 1.0,
  series = NA,
  stringsAsFactors = FALSE
  )

SI.Prefixes <- data.frame(
  name = c("yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo",
    "hecto", "deca", "deci", "centi", "milli", "micro", "nano", "pico", "femto",
    "atto", "zepto", "yocto"),
  multiple = c(1e24, 1e21, 1e18, 1e15, 1e12, 1e9, 1e6, 1e3, 1e2, 10, 0.1, 1e-2,
    1e-3, 1e-6, 1e-9, 1e-12, 1e-15, 1e-18, 1e-21, 1e-24), 
  prefix = c("Y", "Z", "E", "P", "T", "G", "M", "k", "h", "da", "d", "c", "m",
    "µ", "n", "p", "f", "a", "z", "y"),
  stringsAsFactors = FALSE) 


## TODO: Need a "Dimensions -> default Unit" map.

Dimensions <- list(
  length = list(
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



## A reasonably complete set of dimensions and units

## The SI prefixed versions of the base units.
add_unit("M", "g", "gram", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "kg")
add_unit("L", "m", "metre", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "m")
add_unit("T", "s", "second", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "s")
add_unit("I", "A", "ampere", is.coherent = TRUE, gen.prefixes = TRUE, true.basis = "A")
add_unit("Th", "K", "kelvin", is.coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "K")
add_unit("N", "mol", "mole", is.coherent = TRUE, gen.prefixes = TRUE, true.basis =
         "mol") 
add_unit("J", "cd", "candela", is.coherent = TRUE, gen.prefixes = TRUE, true.basis
         = "cd")

## Some dimensions which don't have their own units in SI
add_dimension("velocity", c(length = 1, time = -1)) # m/s
add_dimension("acceleration", c(velocity = 1, time = -1)) # (m/s)/s
add_dimension("area", c(length = 2)) # m^2

## Dimensions with their own units
add_dimension("frequency", c(time = -1))
add_unit("frequency", "Hz", "hertz", "hertz",
         is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("angle", c(length = 1, length = -1)) # rad = m/m
add_unit("angle", "rad", "radian",
         is.coherent = TRUE, gen.prefixes = FALSE)

add_dimension("solid_angle", c(angle = 2)) # sr = rad^2
add_unit("solid_angle", "sr", "steradian",
         is.coherent = TRUE, gen.prefixes = FALSE)

add_dimension("force", c(mass = 1, acceleration = 1)) # N = kg (m/s)/s 
                                        # should perhaps be (momentum = 1, time = -1?)
add_unit("force", "N", "newton", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("pressure", c(force = 1, area = -1)) # Pa = N/m^2
add_unit("pressure", "Pa", "pascal", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("energy", c(force = 1, length = 1)) # J = N m
add_unit("energy", "J", "joule", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("power", c(energy = 1, time = -1)) # W = J/s
add_unit("power", "W", "watt", is.coherent = TRUE, gen.prefixes = TRUE)

add_dimension("electric_charge", c(electric_current = 1, time = -1)) # C = A s
add_unit("electric_charge", "C", "coulomb", is.coherent = TRUE, gen.prefixes =
         TRUE)

add_dimension("voltage", c(energy = 1, electric_charge = -1)) # V = J / C.
add_unit("voltage", "V", "volt", is.coherent = TRUE, gen.prefixes = TRUE)

## Dimensions without their own units
add_dimension("momentum", c(mass = 1, velocity = 1)) # kg (m/s)

## Non-SI energy units

add_unit("energy", "cal", "calorie", multiple = 4.1868, is.coherent = FALSE, gen.prefixes = FALSE)




