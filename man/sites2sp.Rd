\encoding{utf8}
\name{sites2sp}
\alias{sites_to_SpatialPointsDataFrame}
\title{Convert a List of USGS Site Numbers to a SpatialPointsDataFrame}
\description{
Retrieve \dQuote{site file} information for U.S. Geological Survey (USGS) streamgages by site identification number and convert to a \code{SpatialPoints}\code{DataFrame} of the \pkg{sp} package. This function is a trivial wrapper (abstraction layer) on \code{dataRetrieval::}\code{readNWISsite()}, which is the function used to retrieve the site file information from the USGS National Water Information System (NWIS) (U.S. Geological Survey, 2020). The \code{sites_}\code{to_}\code{SpatialPointsDataFrame} streamlines an expected need of \pkg{akqdecay} users requiring spatial representation in the \pkg{sp} package framework, and EPSG declarations are described herein. EPSG Geodetic Parameter Dataset (also EPSG registry) is a public registry of spatial reference systems, Earth ellipsoids, coordinate transformations and related units of measurement.

This function loops site-by-site and queries NWIS instead of groups or ensembles of site numbers---large scale testing indicates that this is a prudent design decision because expected applications could readily send too many site numbers simultaneously to the USGS waterdata services. Also there is a question of how to handle invalid or otherwise \dQuote{bad sites}. The function has \code{try()} wrapped around \code{readNWISsite()} to trap for individual errors---for example, an invalid site number.
}
\usage{
sites_to_SpatialPointsDataFrame(siteNumbers, latlongcrs="+init=epsg:4269",
                                             proj4string=NA,
                                             usesp=TRUE, silent=FALSE, ...)
}
\arguments{
  \item{siteNumbers}{USGS streamgage identification numbers and nomenclature matches that of the \pkg{dataRetrieval} package, and multiple site numbers can be provided;}
  \item{latlongcrs}{A \code{sp::CRS} \emph{string} defining the meaning of the North American Datum of 1983 (NAD83) decimal latitude and longitude values from NWIS and the applicable EPSG spatial reference system is \code{"+init=epsg:4269"} (geographic coordinates, NAD83)(Klokan Technologies, 2020). A call to \code{sp::}\code{CRS(latlongcrs)} is made internally. The authors do not anticipate that the \code{latlongcrs} would ever require explicit redefinition and can be left to the default shown;}
  \item{proj4string}{An \code{sp::CRS()} \emph{string} that will optionally be used for coordinate projection by \code{sp::}\code{spTransform()} and nomenclature matches that of the \pkg{sp} package. A suggested EPSG is \code{"+init=epsg:5070"} (Albers equal area, NAD83) (Klokan Technologies, 2020) and this is suggested by the \code{message()}, if \code{proj4string=NA}. Unprojected data are returned unless the \code{proj4string} is explicitly given;}
  \item{usesp}{A logical to actually trigger \code{sp::}\code{SpatialPointsDataFrame()}, otherwise a \cr merger (total aggregation) of the site information as an \R \code{data.frame} is simply returned. This is provided in case problems with spatial conversion occur;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
A \code{SpatialPointsDataFrame} of the \pkg{sp} package unless \code{usesp=}\code{FALSE} at which point a standard \R \code{data.frame} is returned. For the standard \code{data.frame} option, a vector of bad sites, which are sites for which the web interface of NWIS does not know about, is stored in the attribute triggered as \code{attr("bad sites")}. Bad sites can be those for which the number has never been a USGS number and perhaps these numbers are mistakes on part of the user. There is another possibility. A user might have acquired a valid USGS streamgage identification number for some really old and also likely short record site that did not \dQuote{fully make it} to the digital age. It is possible that internal USGS database access might find some information about these.

Also the \pkg{akqdecay} package is oriented around streamflow processing. To this end, columns related to groundwater site information are removed (see source code). Also, two drainage areas are recorded in the USGS database: total drainage area (\code{drain_area_va}) and contributing drainage area (\code{contrib_drain_area_va}). The pairwise minimum or
\preformatted{
  CDA <- pmin(drain_area_va, contrib_drain_area_va, na.rm=TRUE)
}
of these is computed and placed into a new column \code{CDA} in an effort to have a canonical access point of contributing drainage area. Not all streamgages will have both drainage areas populated in the site file.
}
\author{ W.H. Asquith}
\references{
Klokan Technologies, 2020, EPSG.io—Find coordinate systems worldwide: Unterageri, Switzerland, Klokan Technologies GmbH, digital data, accessed July 15, 2020, at \url{https://epsg.io/}. [Geographic coordinates directly accessible at \url{https://epsg.io/4269}; Albers equal area projection directly accessible at \url{https://epsg.io/5070}.]

U.S. Geological Survey, 2020, National Water Information System, Web interface, accessed July 16, 2020, at \url{https://doi.org/10.5066/F7P55KJN}.
}
\examples{
\dontrun{
ms_sites <- c("07288000", "07288500") # Streamgages on the Sunflower River, MS
spSites  <- sites_to_SpatialPointsDataFrame2(ms_sites, proj4string="+init=epsg:5070")
plot(spSites)

# This example needs to stop at the plot(), but for a stubout of how results
# could be combined to the spatial data frame:
#ms_dv <- new.env(); fill_dvenv(ms_sites,     envir=ms_dv)
#ms_ak <- new.env(); fill_akqenv(dvenv=ms_dv, envir=ms_ak)
#
#LM <- akq_lmom(ms_ak); LM$site_no <- LM$site
#spSites <- merge(spSites, LM)
#head(spSites@data) #}
}
\keyword{spatial data formation}
\keyword{data retrieval}
