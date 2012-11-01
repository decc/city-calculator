\name{ukregco2}
\alias{ukregpop}
\docType{data}
\title{
  UK Regional Emissions Statistics 2005 -- 2010
}

\description{Local authority CO2 emissions and population statistics
  over the period 2005--2010, as published by \acronym{DECC}. The data
  have been reformatted to be in \dQuote{table} format and each local
  authority has been matched with the standard geographical code used by
  \acronym{ONS}.}

\usage{
data(ukregco2)
}

\format{
  Two data frames (\code{ukregco2}, \code{ukregpop}) having the following variables:
  
  \describe{
    \item{\code{yr}}{Year.}
    
    \item{\code{lau}}{Name of the local authority. (Not consistent
      with the regional energy datasets.0}
    
    \item{\code{geography_code}}{Standard \acronym{ONS} 9-character code
      for the geographical region of the local authority. Consistent
      across datasets.}
    
    \item{\code{sector_code}}{(\code{ukregco2} only) A letter code
      identifying the sector.}
    
    \item{\code{sector}}{(\code{ukregco2} only) Economic sector.}
    
    \item{\code{CO2}}{(\code{ukregco2} only) Annual emissions of carbon dioxide,
      thousands tonnes of CO2.}
    
    \item{\code{pop}}{(\code{ukregpop} only) Mid-year population estimate, thousands.}
    
    \item{\code{sta}}{\dQuote{Second Tier Authority}.}
    \item{\code{region}}{\dQuote{Region}. Corresponds to Government Office Regions in
      England, and otherwise to the name of the devolved administration.}
  }
}

\details{

  These datasets are identical to the published files with the addition
  of the standard \acronym{ONS} geographic code to identify the local
  authority. (Some column names have be changed to allow them to be used
  as \R variables. In particular, the variable \code{lau} refers to the
  column called \dQuote{LA Region Code}.)
  
  \subsection{Meaning of a row}{
    
    A row in these data tables is defined uniquely by the combination of
    \code{yr}, \code{lau} (or \code{geography_code}), and either
    \code{sector} (or \code{sector_code}).

  }
  
  \subsection{Effect of the 2009 local government changes}{
    
    On 1 April 2009 there were a number of changes to the administrative
    regions across Great Britain. Several exisiting local authorities
    were abolished and their functions consolidated in either existing
    or newly-created authorities. In the published version of this
    dataset, previous years' emissions have been recalculated for the
    new regions. Note that these changes are treated differently (and
    not consistently) in the present regional energy statistics datasets.
    
  }
  
  \subsection{\acronym{ONS} geographical codes}{

    On the assumption that the regions represent local authorities, the
    author has assigned the new, standard \dQuote{9 character}
    \acronym{ONS} code in the variable \code{geography_code}.

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


\seealso{\code{\link{ukregenergy}} for regional energy statistics.}

\source{
  Department of Energy & Climate Change (2012-08-23), \emph{Local and
    regional CO2 emissions estimates for 2005-2010 -- Full
    dataset}. Downloaded from
  \url{http://www.decc.gov.uk/en/content/cms/statistics/climate_stats/gg_emissions/laco2}
  on 2012-10-25.

}
\references{
  Office for National Statistics, \emph{A Beginner's Guide to UK
    Geography}. \url{http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/administrative/index.html}}

\author{James Geddes \email{james.geddes@decc.gsi.gov.uk}}

\examples{
\dontrun{data(ukregco2)}
}

\keyword{datasets}
