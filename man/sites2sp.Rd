\name{sites2sp}
\alias{sites_to_SpatialPointsDataFrame}
\title{ Convert a List of USGS Site Numbers to a SpatialPointsDataFrame }
\description{
Retrieve \dQuote{site file} information for U.S. Geological Survey (USGS) streamgages by site identification number and convert to a \code{SpatialPoints}\code{DataFrame} of the \pkg{sp} package. This function is a trivial wrapper (abstraction layer) on \code{dataRetrieval::}\code{readNWISsite()}, which is the function used to retrieve the site file information from the USGS National Water Information System (NWIS). The \code{sites_}\code{to_}\code{SpatialPointsDataFrame} streamlines an expected need of \pkg{akqdecay} users requiring spatial representation in the \pkg{sp} package framework. This function loops site-by-site and queries NWIS instead of groups or ensembles of site numbers---large scale testing indicates that this is a prudent design decision because expected applicables can readily send too many site numbers simultaneously to the USGS waterdata services. Also there is a question of how to handle invalid or otherwise \dQuote{bad sites}. The function has \code{try()} wrapped around \code{readNWISsite()} to trap for individual errors---for example, an invalid site number.
}
\usage{
sites_to_SpatialPointsDataFrame(siteNumbers, latlongcrs=NA, proj4string=NA,
                                             usesp=TRUE, silent=FALSE, ...)
}
\arguments{
  \item{siteNumbers}{USGS streamgage identification numbers and nomenclature matches that of the \pkg{dataRetrieval} package, and multiple site numbers can be provided;}
  \item{latlongcrs}{A \code{sp::CRS} \emph{string} defining the meaning of the decimal latitude and longitude values from NWIS. A call to \code{sp::}\code{CRS()} is made internally;}
  \item{proj4string}{An \code{sp::CRS()} \emph{string} that will optionally be used for coordinate projection via \code{sp::}\code{spTransform()} and nomenclature matches that of the \pkg{sp} package;}
  \item{usesp}{A logical to actually trigger \code{sp::}\code{SpatialPointsDataFrame()}, otherwise a \cr merger (total aggregation) of the site information as an \R \code{data.frame} is simply returned. This is provided in case problems with spatial conversion occur;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  A \code{SpatialPointsDataFrame} of the \pkg{sp} package unless \code{usesp=}\code{FALSE} at which point a standard \R \code{data.frame} is returned. For the standard \code{data.frame} option, a vector of bad sites, which are sites for which the web interface of NWIS does not know about, is stored in the attribute triggered as \code{attr("bad sites")}. Bad sites can be those for which the number has never been a USGS number and perhaps these numbers are mistakes on part of the user. There is another possibility. A user might have acquired a valid USGS streamgage identification number for some really old and also likely short record site that did not \dQuote{fully make it} to the digital age. It is possible that internal USGS database access might find some information about these.
}
\author{ W.H. Asquith}
\examples{
\dontrun{
library(sp) # to gain access to plot() for sp objects
ms_sites <- c("07288000", "07288500") # Streamgages on the Sunflower River, MS
spSites  <- sites_to_SpatialPointsDataFrame(ms_sites)
plot(spSites) # }

\dontrun{
ms_dv <- new.env(); ms_ak <- new.env();
fill_dvenv(ms_sites, envir=ms_dv); fill_akqenv(ms_dv, envir=ms_ak)
LM <- akq_lmom(ms_ak); LM$site_no <- LM$site
spSites <- merge(spSites, LM) #}
}
\keyword{spatial data formation}
\keyword{data retrieval}
