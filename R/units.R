### Units - A physical quantity package for R

## Notes:
##
## - Ratio scales only -- no degrees Celsius to Kelvin
## - Uses SI as the underlying basis

## An object of class Quantity contains an attribute "measure", where
##
## atomic_unit := one of a list of named units (kg, J, N, Gt, mHz, ...)

is.basis_unit <- function(au) {
  au %in% Units$basis$symbol
}

is.coherent_unit <- function(au) {
  au %in% Units$coherent$symbol | is.basis_unit(au)
}

is.atomic_unit <- function(au) {
  au %in% Units$other$symbol | is.coherent_unit(au)
}

## unit := vector-of (atomic_unit = power) such that no atomic_unit occurs twice and no
## power is zero

is.unit <- function(u) {
  (is.numeric(u)
   && all(is.atomic_unit(names(u))) 
   && !anyDuplicated(names(u))
   && !any(u == 0))
}

## A unit is a compatible unit for a quantity kind just in case the reduction of
## the dimensions of the unit to basis dimensions is the same as the reduction
## of the dimensions of the quantity kind.

is.compatible_unit <- function(unit, qk) {
  
}

simplify.dimension <- function(qk) {}


## 
## atomic_measure := pair (unit, quantity_kind) such that unit is a compatible unit for quantity_kind
## 
## measure := list-of (atomic_measure, power) such that no power is zero
##                
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


## Determine whether argument is an atomic unit

## Determine whether argument is a unit

## Determine whether argument is an atomic measure


## Determine whether argument is a measure
is.measure <- function(m) {
  
}

as.Quantity <- function(vec, measure) {
  
}

print.Quantity <- function(vec, verbose = FALSE) {
  print(paste("It's ... ", format(vec)))
  invisible(xx)
}


q_to_unit <- function(vec, new.unit) {}

q_add <- function(q1, q2) {} # Can only add q's with identical measures
q_mult <- function(q1, q2, kind = NULL) {}


as_vector <- function(dim_expr) {
    Reduce(`+`,                                           # Add up ...
           Map(`*`,                                       # the powers of ..
               dim_expr,                            
               lapply(Dimensions[names(dim_expr)],     # the basis dimensions
                      function(x) {`[[`(x, "vector")} ))) # (extracting the vectors).   
}

add_dimension <- function(name, definition) {
  if (name %in% names(Dimensions)) {
    stop("dimension [", name, "] already defined")
  }
  Dimensions[[name]] <<- list(definition = definition,
                                  vector = as_vector(definition))
}


add_unit0 <- function(dimension, symbol, name, plural.name, coherent, multiple, series) {

  if (any(is.basis_unit(symbol))) {
    stop("unit '", symbol, "' is already defined (as a basis unit)")
  } else if (any(is.coherent_unit(symbol))) {
    stop("unit '", symbol, "' is already defined (as a coherent unit)")
  } else if (any(is.atomic_unit(symbol))) {
    stop("unit '", symbol, "' is already defined")
  }

  if (coherent) {
    Units$coherent <<- rbind(Units$coherent,
                             data.frame(dimension = dimension,
                                        symbol = symbol,
                                        name = name,
                                        plural.name = plural.name))
  } else {
    Units$other <<- rbind(Units$other,
                          data.frame(dimension = dimension,
                                     symbol = symbol,
                                     name = name,
                                     plural.name = plural.name,
                                     series = series,
                                     multiple = multiple))
  }
}



## Add units, including making up all the prefixed versions, calling add_unit0
## for each individual unit (and to check for duplicates).

add_unit <- function(dimension, symbol, name, plural.name = "", coherent = FALSE, multiple
                     = 1.0, gen.prefixes = FALSE, true.basis = NA) {
  
  if (plural.name == "") plural.name <- paste(name, "s", sep = "")
  
  if (!gen.prefixes) {
    add_unit0(dimension, symbol, name, plural.name, coherent, multiple, NA) 
  } else {
    
    ## Prefixed units are added one at a time so that add_unit0 can report
    ## sensible errors on duplicates. We don't expect new units to be added
    ## very frequently.

    if (is.na(true.basis)) {
      add_unit0(dimension, symbol, name, plural.name, coherent, multiple, symbol)
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else if (true.basis == symbol) {
      prefix.range <- 1:length(SI.Prefixes$prefix)
      skip.multiple <- 1
    } else {
      add_unit0(dimension, symbol, name, plural.name, FALSE, multiple, symbol)
      skip <- match(true.basis, paste(SI.Prefixes$prefix, symbol, sep = ""))
      prefix.range <- (1:length(SI.Prefixes$prefix))[-skip]
      skip.multiple <- SI.Prefixes$multiple[[skip]]
    }
    
    for (i in prefix.range) {
      add_unit0(dimension,
                paste(SI.Prefixes$prefix[[i]], symbol, sep = ""),
                paste(SI.Prefixes$name[[i]], name, sep = ""),
                paste(SI.Prefixes$name[[i]], plural.name, sep = ""),
                FALSE, # These are not coherent, derived units
                SI.Prefixes$multiple[[i]] / skip.multiple,
                symbol)
    }
  }
}


## Definitions of quantity kinds
## Should be package local in the end

Basis.Dimensions <- data.frame(
  symbol = c("L", "M", "T", "I", "Th", "N", "J"),
  name = c("length", "mass", "time", "electric current",
    "thermodynamic temperature", "amount of substance", "luminous intensity"),
  dimension = c("length", "mass", "time", "electric_current",
    "temperature", "amount", "luminous_intensity")) 

## Units in basis.units are the default for those dimensions whose
## definition is NULL. The order should be precisely the same as the order in
## Basis.Dimensions

Units <- list(
  basis = data.frame(
    symbol = c("m", "kg", "s", "A", "K", "mol", "cd"),
    dimension = c("length", "mass", "time", "electric_current",
      "temperature", "amount", "luminous_intensity"), 
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
## Same format, but now each unit lists its known Dimensions.
## And also need a "default units map Dimensions -> Units$.

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

format_vector <- function(vector) {
    paste(
        unlist( # Drops any NULLs returned by format_unit 
               mapply(format_unit, Units[[c("basis", "symbol")]],
                      vector)),
        collapse = " ")
}


## A reasonably complete set of dimensions and units

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
add_dimension("velocity", c(length = 1, time = -1)) # m/s
add_dimension("acceleration", c(velocity = 1, time = -1)) # (m/s)/s
add_dimension("area", c(length = 2)) # m^2

## Dimensions with their own units
add_dimension("frequency", c(time = -1))
add_unit("frequency", "Hz", "hertz", "hertz",
         coherent = TRUE, gen.prefixes = TRUE)

add_dimension("angle", c(length = 1, length = -1)) # rad = m/m
add_unit("angle", "rad", "radian",
         coherent = TRUE, gen.prefixes = FALSE)

add_dimension("solid_angle", c(angle = 2)) # sr = rad^2
add_unit("solid_angle", "sr", "steradian",
         coherent = TRUE, gen.prefixes = FALSE)

add_dimension("momentum", c(mass = 1, velocity = 1)) # kg (m/s)
add_dimension("force", c(mass = 1, acceleration = 1)) # N = kg (m/s)/s 
                                        # should perhaps be (momentum = 1, time = -1?)
add_dimension("pressure", c(force = 1, area = -1)) # Pa = N/m^2
add_dimension("energy", c(force = 1, length = 1)) # J = N m
add_dimension("power", c(energy = 1, time = -1)) # W = J/s
add_dimension("electric_charge", c(electric_current = 1, time = -1)) # C = A s
add_dimension("voltage", c(energy = 1, electric_charge = -1)) # V = J / C.


