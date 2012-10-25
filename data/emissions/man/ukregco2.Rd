\name{ukregco2}
\alias{ukregco2}
\docType{data}
\title{
UK Regional Carbon Dioxide Emissions 2005 -- 2010
}
\description{
Local authority emissions statistics, as published by \acronym{DECC}, covering the
period 2005--2010. The data have been reformatted to be in \dQuote{flat
  table} format. In addition, each local authority has been matched with
the standard geographical code used by \acronym{ONS}. 
}
\usage{data(ukregco2)}
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
    
    \item{\code{CO2}}{The annual emissions of carbon dioxide in
      thousands tonnes of CO2 (kt CO2).}
  }
}

\details{

  The geographical codes used are those which were in existence at the
  time this data set was reformatted (25 August 2012). It cannot be
  guaranteed that the regions described by those codes match precisely
  the regions used when compiling the emissions data set.

  The kind of local authority depends on the region; however, the full
  set of local authorities forms a complete, non-overlapping cover of
  the \acronym{UK}. The kinds of local authority are:
  
  \tabular{lll}{
    \bold{Coding scheme} \tab \bold{Entity type} \tab \bold{Region}
    \cr
    \verb{E06}\ldots \tab Unitary Authorities \tab England \cr
    \verb{E07}\ldots \tab Non-metroplitan Districts \tab England \cr
    \verb{E08}\ldots \tab Metropolitan Districts \tab England \cr
    \verb{E09}\ldots \tab London Boroughs \tab England \cr
    \verb{W06}\ldots \tab Unitary Authorities \tab Wales \cr
    \verb{S12}\ldots \tab Council Areas \tab Scotland \cr
    \verb{DCA92CD} \tab District Council Areas \tab Northern Ireland
  }
  Note that Northern Ireland has opted not to use the \acronym{ONS} standard
  9-digit coding sceheme.
  
  The codes were determined by matching the names in \code{la_name} with
  the standard names of each geography as published by
  \acronym{ONS}. The names matched exactly apart from the following
  discrepancies:
  
  \tabular{ll}{
    \bold{Original file} \tab \bold{Standard Geography} \cr
    \verb{Durham} \tab \verb{County Durham} \cr
    \verb{St. Albans} \tab \verb{St Albans} \cr
    \verb{St. Edmundsbury} \tab \verb{St Edmundsbury} \cr
    \verb{Vale of Glamorgan, The} \tab \verb{The Vale of Glamorgan} \cr
    \verb{Rhondda, Cynon, Taff} \tab \verb{Rhondda Cynon Taff} \cr
    \verb{Edinburgh, City of} \tab \verb{City of
      Edinburgh} \cr
    \verb{Derry} \tab \verb{Derry City} \cr
    \verb{Dungannon and South Tyrone} \tab \verb{Dungannon}
  }
}

\seealso{\code{\link{ukregpop}} for regional population statistics from
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
data(ukregco2)
}
\keyword{datasets}
