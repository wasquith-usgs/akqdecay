\name{dvget}
\alias{dvget}
\title{ Retrieve USGS Discharge Daily Values structured for Asquith--Knight Discharge Decay Analyses }
\description{
Retrieve U.S. Geological Survey (USGS) daily mean-streamflow values for a streamgage identification number. This function is a wrapper on \code{dataRetrieval::}\code{readNWISdv()} to provide an abstraction layer to the \pkg{dataRetrieval} package. The \code{dvget} function adds some additional information for purpose of the \pkg{akqdecay} package. The function creates the canonical daily-mean streamflow table (an \R \code{data.frame}) to be picked up in turn by the \code{\link{akqdecay}} function of this package. The function \code{\link{fill_dvenv}} is a wrapper that can be used to fill an \R \code{environment} with the output of the \code{\link{akqdecay}} function.
}
\usage{
dvget(siteNumber, sdate="",   edate="", flowlo=NULL, flowhi=NULL,
                  date2s=NA,  date2e=NA, ignore.working=TRUE,
                  ignore.provisional=TRUE, silent=FALSE,
                  drsilent=TRUE, drget=FALSE, pCode="00060", sCode="00003",
                  message="", ...)
}
\arguments{
  \item{siteNumber}{USGS streamgage identification number and nomenclature matches that of the \pkg{dataRetrieval} package. Multiple site numbers can be provided because the underlying \code{dataRetrieval::}\code{readNWISdv()} supports that and the resulting behavior should be about the same between these two functions. A warning message though is shown. This is important because \code{\link{akqdecay}} is not vectorized to handle multiple streamgages and will trigger a \code{stop()} in execution---Multiple streamgages should be processed through \code{\link{fill_dvenv}} and \code{\link{fill_akqenv}};}
  \item{sdate}{Start date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed;}
  \item{edate}{Ending date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed;}
  \item{flowlo}{Optional lower streamflow threshold on which to convert to \code{NA} (to keep continuous day time stamps) through \eqn{Q >} \code{flowlo} \eqn{\rightarrow} \code{NA};}
  \item{flowhi}{Optional upper streamflow threshold on which to convert to \code{NA} (to keep continuous day time stamps) through \eqn{Q <} \code{flowhi} \eqn{\rightarrow} \code{NA};}
  \item{date2s}{An optional start date of record (greater than and equal to) in the same format as \code{sdate}. This option is only used \bold{after} the \code{readNWISdv()} function has retrieved the data; this option is provided for accommodating situations in which \code{readNWISdv()} has difficulties in pulling the correct data. This option is provided on an experimental basis because the author has become aware that sites with multiple daily-streamflow data descriptors do not have similar behavior when \code{sdate} is attempted for the start of record. The issues causing the inclusion of this option otherwise difficult to explain---please contact the authors as needed.}
  \item{date2e}{An optional ending date of record (less than and equal to) in the same format as \code{edate}. This option is only used \bold{after} the \code{readNWISdv()} function has retrieved the data; this option is provided for accommodating situations in which \code{readNWISdv()} has difficulties in pulling the correct data. This option is provided on an experimental basis because the author has become aware that sites with multiple daily-streamflow data descriptors do not have similar behavior when \code{edate} is attempted for the start of record. The issues causing the inclusion of this option otherwise difficult to explain---please contact the authors as needed.}
  \item{ignore.working}{The USGS identifies at least \dQuote{Approved} (\code{A}), \dQuote{Provisional} (\code{P}), and \dQuote{Working} (\code{W}) record types. The default triggers the deletion of data rows flagged as working record;}
  \item{ignore.provisional}{The USGS identifies at least \dQuote{Approved} (\code{A}), \dQuote{Provisional} (\code{P}), and \dQuote{Working} (\code{W}) record types. The default triggers the deletion of data rows flagged as provisional record;}
  \item{silent}{Suppress informative calls to \code{message()} with the \code{message};}
  \item{drsilent}{The argument though converted to \dQuote{silent} that is passed to \code{try} for a level of informative error trapping on top of \code{dataRetrieval::}\code{readNWISdv()};}
  \item{drget}{If set, the retrieval from \code{dataRetrieval::}\code{readNWISdv()} is immediately returned following the internal call to \code{dataRetrieval::}\code{renameNWISColumns()};}
  \item{pCode}{Parameter code (default is discharge [streamflow]) and nomenclature almost matches that of the \pkg{dataRetrieval} package;}
  \item{sCode}{Statistic code (default is daily mean) and nomenclature almost matches that of the \pkg{dataRetrieval} package}
  \item{message}{An optional string that if populated will trigger a \code{message()} call that a user might find useful in massive batch processing operations; and}
  \item{...}{Additional arguments to pass to function \code{dataRetrieval::}\code{readNWISdv()}.}
}
\value{
  An \R \code{data.frame} is returned with these expected columns. Some streamgages can have multiple discharges descriptors typed as daily values. Only the \code{Flow} column is handled by \pkg{akqdecay}.
  \item{agency_cd}{The agency code for the data;}
  \item{site_no}{The streamgage identification number;}
  \item{Date}{The date, note that the capitalization is from the \pkg{dataRetrieval} package, elsewhere in \pkg{akqdecay} this will become lower case;}
  \item{Flow}{The streamflow in cubic feet per second, note that the capitalization is from the \pkg{dataRetrieval} package, elsewhere in \pkg{akqdecay} this will become lower case or an alternative name for streamflow;}
  \item{Flow_cd}{A coding system for the streamflow (\code{A}, approved record; \code{P}, provisional, and \code{W}, working record);}
  \item{site}{A character representation of the \code{site_no}, which likely is already a character. The reasoning for having another site column is in case a user need some type of flexibility in later processing. The user could freely replace contents of this column;}
  \item{year}{The calendar year of the date;}
  \item{decade}{The decade of the daily value. The decade is assign by taking the year and the trailing digit has been stripped and replaced with zero. This is \emph{not} a technique in which a \dQuote{decade} is centered on an even step of 10---meaning, say that 1996--2005 \emph{is not the} \dQuote{2000 decade} but simply 01/01/2000--12/31/2009 \emph{is the} \dQuote{2000 decade;}}
  \item{wyear}{The water year; and}
  \item{month}{The month.}
}
\note{
For the greater purposes of the \pkg{akqdecay} package, the arguments \code{pCode} and \code{sCode} for their defaults are expected to be left untouched. The capitalization inconsistency in the returned \R \code{data.frame} is left intact as it is consistent with the operation of the \code{dataRetrieval::}\code{renameNWISColumns()} function that is called internally. Lastly, at least one streamgage (07040000 in 2015 [as for Nov. 2017 testing]) has been found in massive-scale testing that has -999999 for a daily flow. Such values are converted to \code{NA}.
}
\author{ W.H. Asquith}
\references{
Hirsch, R.M., and De Cicco, L.A., 2015, User guide to Exploration and Graphics for RivEr Trends (EGRET) and dataRetrieval: R packages for hydrologic data (version 2.0, February 2015): U.S. Geological Survey Techniques and Methods book 4, chap. A10, 93 p., \url{http://dx.doi.org/10.3133/tm4A10}
}
\seealso{\code{\link{akqdecay}}, \code{\link{fill_dvenv}}, \code{\link{gsid2str}}
}
\examples{
\dontrun{
# USGS 14362000 Applegate River near Copper, Oregon
Copper <- dvget("14362000", sdate="1940-01-01", edate="1940-01-31")
print(Copper) # An inspection of the retrieved daily values. #}
}
\keyword{data retrieval}
\keyword{daily streamflow}
