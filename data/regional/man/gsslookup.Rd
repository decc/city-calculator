\name{gsslookup}
\alias{gsslookup}
\docType{data}
\title{
Names and Codes for UK Administrative Geography
}
\description{
  
  GSS codes and names for local authorities in Great Britain plus
  district council areas in Northern Ireland (which uses a different
  coding scheme). Includes local authorities which no longer exist
  following the structural boundary changes on 1 April 2009.

}

\usage{data(gsslookup)}

\format{
  
  A data frame with the following 7 variables, having one row per
  geographic region.
  
  \describe{
    
    \item{\code{geography_code}}{The new \dQuote{9 character} GSS code (or
      the old DCA92 code for district council areas in Northern Ireland).}
    
    \item{\code{geography_name}}{The name of the local authority.}
        
    \item{\code{geography_welsh_name}}{The name of the region in Welsh.}
    
    \item{\code{operation_date}}{The date the code was introduced.}
    
    \item{\code{termination_date}}{The date after which the code was no longer current.}
    
    \item{\code{parent_geography}}{The GSS code of the next higher
      administrative region.}
    
    \item{\code{entity_code}}{The GSS entity code for the type of region.}
    
  } 

} 

\details{

  This dataset provides a lookup from GSS codes to names. It includes
  those codes in existence since 2009 and until July 2012. Northern
  Ireland has elected not to use the new \acronym{ONS} \dQuote{9 character}
  coding scheme; however, those codes have been added to the list, although
  strictly speaking they do not have the same status. 

}

\source{
  ONS, \emph{Code History Database}, July 2012, 
  \url{http://www.ons.gov.uk/ons/guide-method/geography/products/names--codes-and-look-ups/code-history-database/index.html}
  }
  
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(gsslookup)
}
\keyword{datasets}
