\name{ukregpop}
\alias{ukregpop}
\docType{data}
\title{
UK Regional Population Statistics 2005 -- 2010
}
\description{
  Local authority mid-year population estimates, published by
  \acronym{DECC} alongside the regional emissions statistics. The data
  have been reformatted to be in \dQuote{flat table} format. In addition,
  each local authority has been matched with the standard geographical
  code used by \acronym{ONS}.  }
\usage{data(ukregpop)}
\format{
  A data frame with 34272 observations on the following 8 variables.
  \describe{
    
    \item{\code{la_name}}{The human-readable name of the local
      authority as in the original file published by \acronym{DECC}. The
      original name of this field was \dQuote{LA Region Name}.}
    
    \item{\code{geography_code}}{The standard \acronym{ONS}
      code for the geographical region of the local authority.}
    
    \item{\code{sta_name}}{The \dQuote{Second Tier Authority}
      from the original dataset.}
    
    \item{\code{region_name}}{The \dQuote{Region} from the
      original dataset. Corresponds to Government Office Regions in
      England, and otherwise to the name of the devolved administration.}
    
    \item{\code{yr}}{The year.}
  
    \item{\code{sector_code}}{A letter code identifying the emissions sector.}
    
    \item{\code{sector_name}}{The name of the emissions sector.}
    
    \item{\code{pop}}{Mid-year population estimate, thousands.}
  }
}

\details{
For further information on the geographical breakdown, see \code{\link{ukregco2}}.
}

\seealso{\code{\link{ukregco2}} for regional emissions statistics from
  the same \acronym{DECC} publication.}

\source{
  Deptartment of Energy & Climate Change (2012-08-23), \emph{Local and
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
data(ukregpop)
}
\keyword{datasets}
