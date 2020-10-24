\encoding{utf8}
\name{sites_by_state}
\alias{sites_by_state}
\title{Get a List of USGS Site Numbers by State (or All States) }
\description{
Retrieve \dQuote{site file} information for U.S. Geological Survey (USGS) streamgages by state abbreviation following the abbreviations in \code{state.abb} as part of base \R. This function is a trivial wrapper (abstraction layer) on both \code{dataRetrieval::}\code{whatNWISsites()} and \code{dataRetrieval::}\code{readNWISsite()}, which are the functions used to retrieve sites by state and the site file information from the USGS National Water Information System (NWIS) (U.S. Geological Survey, 2020), respectively.
}
\usage{
sites_by_state(states, parameterCd="00060", site_tp_cd="ST",
               agency_cd="USGS", ditch_15digit=TRUE,
               rm_alt=TRUE, rm_coord=TRUE, rm_countystate=TRUE,
               rm_huc=TRUE, rm_tz=TRUE,
               rm_drain=TRUE, make_CDA=TRUE,
               simplify=TRUE, strip_single_quote_mark=TRUE,
               silent=FALSE, ...)
}
\arguments{
  \item{states}{A vector of two-character state abbreviation and each element is compared to the list of \code{datasets::}\code{state.abb};}
  \item{parameterCd}{The parameter code (default is discharge) of NWIS and the \pkg{dataRetrieval} package;}
  \item{site_tp_cd}{The site type code (default is stream) of NWIS and the \pkg{dataRetrieval} package;}
  \item{agency_cd}{The agency code to exclusively retain in the retrieved site files by state;}
  \item{ditch_15digit}{A logical to trigger removal of site numbers having 15 digits. This feature permits the removal of site numbers constructed of latitude and longitudes. Such surface water sites are almost always(?) some type of special project site and lack extensive records in time for the statistical analyses designed into the \pkg{akqdecay} package;}
  \item{rm_alt}{A logical to trigger removal of altitude related columns;}
  \item{rm_coord}{A logical to trigger removal of horizontal coordinate related columns. Note that the decimal latitude and longitude are always retained and are in the North American Datum of 1983 (NAD83);}
  \item{rm_countystate}{A logical to trigger removal of county, state, and country code columns;}
  \item{rm_huc}{A logical to trigger removal of the hydrologic unit code column;}
  \item{rm_tz}{A logical to trigger removal of the time-zone code column;}
  \item{rm_drain}{A logical to trigger removal of the total and contributing drainage area columns, but this trigger is not related to the \code{makeCDA};}
  \item{makeCDA}{A logical to trigger the pairwise minimum of the two drainage areas and create a column titled \code{CDA};}
  \item{simplify}{A logical to trigger the removal of many columns for which are quite unlikely of be of general interest in any type of \dQuote{site table} construction for a publication;}
  \item{strip_single_quote_mark}{A logical to trigger the removal of single quotation marks. This is done so that should the table be exported to a delimited file that might not be using the double quotes correction, that it remains likely the table is still parsable. For example, there is a town in Texas called \dQuote{D'Hanis} and is in a few streamgage names. This name quietly gets converted to \dQuote{DHanis};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
A data frame of the USGS site information file for stations in the given states with variable columns but reduction in columns from those retrieved by a straight call to \code{dataRetrieval::}\code{readNWISsite()}. Two drainage areas are recorded in the USGS database: total drainage area (\code{drain_area_va}) and contributing drainage area (\code{contrib_drain_area_va}). The pairwise minimum or
\preformatted{
  CDA <- pmin(drain_area_va, contrib_drain_area_va, na.rm=TRUE)
}
of these is computed and placed into a new column \code{CDA}, if \code{makeCDA} is set. This is done in an effort to have a canonical access point of contributing drainage area. Not all streamgages will have both drainage areas populated in the site file.
}
\author{ W.H. Asquith}
\references{
U.S. Geological Survey, 2020, National Water Information System, Web interface, accessed July 16, 2020, at \url{https://doi.org/10.5066/F7P55KJN}.
}
\examples{
# This is a very convenient recipe for production of a "site table" for a
# publication using streamgages aligned to study area boundary of states.
SitesInMyStudy <- sites_by_state(c("RI","CT", "purpose mistake"))
print(head(SitesInMyStudy)) # agency_cd, site_no, station_nm,
                            # dec_lat_va, dec_long_va, and CDA
nm <- c("Agency_code", "Streamgage_identification_number", "Streamgage_name",
        "Decimal_latitude_NAD83", "Decimal_longitude_NAD83",
        "Contributing_drainage_area")
names(SitesInMyStudy) <- nm
}
\keyword{data retrieval}
