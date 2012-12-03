\name{ukregenergy}
\alias{ukreggas}
\alias{ukreggas_excluded}
\alias{ukregelectricity}
\alias{ukregroadfuel}
\alias{ukregotherfuel}
\docType{data}
\title{
UK Regional Energy and Emissions Statistics 2005 -- 2010
}

\description{Local authority energy consumption statistics over the
period 2005--2010, as published by \acronym{DECC}. The data have been
reformatted to be in \dQuote{table} format and each local authority has
been matched with the standard geographical code used by \acronym{ONS}.}

\usage{
data(ukregenergy)
}

\format{

  Six data frames: \code{ukregenergy} (amalgmated energy data with
  consistent units), \code{ukreggas}, \code{ukreggas_excluded},
  \code{ukregelectricity}, \code{ukregroadfuel}, and
  \code{ukregotherfuels}.
  
  \subsection{Variables identifying an observation}{
    \describe{
      \item{\code{yr}}{Year.}
      
      \item{\code{lau}}{Name of the local authority. Not consistent between datasets.}
      
      \item{\code{geography_code}}{Standard \acronym{ONS} 9-character code
	for the geographical region of the local authority. Consistent
	across datasets.}
    
      \item{\code{sector}}{Economic sector. Not consistent across all
	datasets.}

      \item{\code{sector0}}{High-level economic sector.}
  
      \item{\code{vehicle_type}}{(\code{ukregroadfuel}) Type of vehicle (and
	sometimes fuel).}
      \item{\code{fuel}}{(\code{ukregotherfuels}) Fuel type.}
    }
  }
  
  \subsection{Value variables}{
    \describe{
      \item{\code{energy}}{(Combined data set) Energy consumption.}
      \item{\code{GWh}}{(Gas, electricity) Energy consumption, GWh.}
      \item{\code{ktoe}}{(Other fuels) Energy consumption,
	thousand tonnes of oil equivalent.}
      \item{\code{thousand-tonnes}}{(Road fuel) Road fuel
	consumption, thousand tonnes.}
      \item{\code{N_consumers}}{(\code{ukreggas}) Number of consumers, thousands.}
      \item{\code{N_MPAN}}{(\code{ukregelectricity}) Number of Meter Point
	Administration Numbers, thousands.}
      \item{\code{power-stations}}{(\code{ukreggas_excluded}) Number of
	power stations whose consumption is not included in the data in
	\code{ukreggas}.}
      \item{\code{large-industrial-consumers}}{(\code{ukreggas_excluded})
	Number of large industrial consumers whose consumption is not
	incuded in the data in \code{ukreggas}.}
    }
  }
  
  \subsection{Other variables}{
    \describe{
      \item{\code{NUTS4_code}}{One or more NUTS4 codes (which have been
	abolished) describing the geographical region.}
      \item{\code{LAU1_code}}{The old 4-character \acronym{ONS} geographical region code.}
    }
  }
}

\details{

  These datasets are indentical to the files for local authority
  energy consumption statistics as published by
  \acronym{DECC} with the addition of the standard \acronym{ONS}
  geographic code to identify the local authority. See below for details
  of each dataset. (Some column names have be changed to allow them to be
  used as \R variables. In particular, the variable \code{lau} refers to
  the column called \dQuote{LA Region Code}.)

  \subsection{Coverage}{
    Not all periods and regions are covered in all datasets. The
    following table gives details, and indicates how the dataset handles
    the 2009 local government changes. (For completeness, the emissions
    dataset, \code{\link{ukregco2}}, is also shown.).
    \tabular{llll}{
      \bold{Dataset} \tab \bold{Period} \tab \bold{Coverage} \tab \bold{Regions} \cr
      Emissions \tab 2005--2010 \tab UK \tab All years use new
      geographies. \cr
      Gas \tab 2005--2010 \tab GB \tab 2010 uses new
      geographies. \cr
      Electricity \tab 2005--2010 \tab GB  \tab 2010 uses new
      geographies \cr
      Road Fuel \tab 2005--2010 \tab UK \tab All years use old
      geographies \cr
      Other Fuels \tab 2005--2009 \tab UK \tab All years use old
      geographies \cr
    }
  }
  
  \subsection{Meaning of a row}{
    A row in these data tables is defined
    uniquely by the combination of \code{yr}, \code{lau} (or
    \code{geography_code}), \code{sector}
    or \code{vehicle_type} (if present), and \code{fuel} (if present).
  }
  
  \subsection{Effect of the 2009 local government changes}{

    On 1 April 2009 there were a number of changes to the administrative
    regions across Great Britain. Several exisiting local authorities
    were abolished and their functions consolidated in either existing
    or newly-created authorities. These changes are treated differently
    in different datasets (see the table above). To obtain a consistent
    set of geographies for all datasets, you should consolidate the old
    regions where necessary. A list of the changes is available from the
    \acronym{ons}.
    
  }
  
  \subsection{\acronym{ONS} geographical codes}{

    The datasets as published contain inconsistencies in the names and
    codes of the local authorities. (It is implied that the regions are
    NUTS4 regions but, for example, Chester-le-Street is given the same
    NUTS4 code as County Durham, the unitary authority which subsumed
    it.) On the assumption that the regions represent local authorities,
    the author has assigned the new, standard \dQuote{9 character}
    \acronym{ONS} in the variable \code{geography_code}.

    The \acronym{ONS} codes used are those which were in existence at
    the end of 2010. They can be used, with caution, to match local
    authorities across datasets and years. Note, however, that no
    attempt has been made to check whether there were any boundary
    changes over the life of these datasets which did not affect the
    name of the local authority.

    The full set of local authorities forms a complete, non-overlapping
    cover of the \acronym{UK}. There are several different kinds:
    
    \tabular{lll}{
      \bold{Coding scheme} \tab \bold{Entity type} \tab \bold{Region}
      \cr
      \verb{E06}\ldots \tab Unitary Authorities \tab England \cr
      \verb{E07}\ldots \tab Non-metroplitan Districts \tab England \cr
      \verb{E08}\ldots \tab Metropolitan Districts \tab England \cr
      \verb{E09}\ldots \tab London Boroughs \tab England \cr
      \verb{W06}\ldots \tab Unitary Authorities \tab Wales \cr
      \verb{S12}\ldots \tab Council Areas \tab Scotland \cr
      \verb{95} \ldots \tab District Council Areas \tab Northern Ireland
    }
    Note that Northern Ireland has opted not to use the \acronym{ONS} standard
    9-digit coding sceheme.
  }
}

\section{Gas}{

  The user should be aware of a number of definitional issues in the gas
  dataset.

  \subsection{Gas years}{Gas consumption data is measured over a \dQuote{gas year},
    which is a period of time from 1 October to the end of the following
    September. The gas year given is the year in which the most months
    fall. For example, gas year 2011 refers to the period beginning 1
    October 2010 and ending 30 September 2011.}
  
  \subsection{Weather adjustment}{The gas consumption data is not raw
  consumption data but has been \dQuote{weather normalised} by the data
  provider.}

  \subsection{Sector definition}{Gas consumers are classified into
  sectors based on their annual usage: consumers whose annual
  consumption is 73,200 kWh or lower are classified as
  \dQuote{Domestic}, those with annual consumption over 73,200 kWh are
  classified as \dQuote{Commercial and Industrial}.}
  
} 

\section{Electricity}{The original source dataset contains, in adition,
figures for the average consumption per household.}

\section{Other Fuels}{Industrial petroleum consumption includes
  consumption within industry and the energy sectors including oil
  refineries. Agriculture petroleum consumption excludes
  propane. Industrial & Commercial coal consumption includes some coal
  consumption by auto-generators. Renewables & Waste excludes that used
  for electricity generation.}

\seealso{\code{\link{ukregco2}} for regional emissions statistics.}

\source{

  Department of Energy & Climate Change (2012), \emph{Sub-national energy consumption statistics}. Downloaded
  from
  \url{http://www.decc.gov.uk/en/content/cms/statistics/energy_stats/regional}
  on 2012-10-31.
}
\references{
  Office for National Statistics, \emph{A Beginner's Guide to UK
    Geography}. \url{http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/administrative/index.html}}

\author{James Geddes \email{james.geddes@decc.gsi.gov.uk}}

\examples{
\dontrun{data(ukregenergy)}
}

\keyword{datasets}
