\encoding{utf8}
\name{mnget}
\alias{mnget}
\title{Retrieve USGS Discharge Monthly Values structured in the spirit of this package}
\description{
Retrieve U.S. Geological Survey (USGS) daily mean-streamflow values for a streamgage identification number. This function is a wrapper on \code{dataRetrieval::}\code{readNWISstat()} to provide an abstraction layer to the \pkg{dataRetrieval} package. The \code{mnget} function adds some additional information for purpose of the \pkg{akqdecay} package. The function creates the canonical daily-mean streamflow table (an \R \code{data.frame}) to be picked up in turn by the \code{\link{akqdecay}} function of this package. The function \code{\link{fill_mnenv}} is a wrapper that can be used to fill an \R \code{environment} with the output of the \code{\link{akqdecay}} function.  Users interested in monthly values might be especially interested in the monthly computations based on the daily streamflows but with the inclusion of separated streamflow in \code{\link{dvpart}} and \code{\link{fill_dvpartenv}} (see \bold{Examples} in \code{\link{dvpart}} for a demonstration).
}
\usage{
mnget(siteNumber, sdate="", edate="", flowlo=NULL, flowhi=NULL,
                  silent=FALSE,
                  drsilent=TRUE, drget=FALSE, pCode="00060", message="", ...)
}
\arguments{
  \item{siteNumber}{USGS streamgage identification number and nomenclature matches that of the \pkg{dataRetrieval} package. Multiple site numbers can be provided because the underlying \code{dataRetrieval::}\code{readNWISstat()} supports that and the resulting behavior should be about the same between these two functions. A warning message though is shown. This is important because \code{\link{akqdecay}} is not vectorized to handle multiple streamgages and will trigger a \code{stop()} in execution---Multiple streamgages should be processed through \code{\link{fill_mnenv}} and \code{\link{fill_akqenv}};}
  \item{sdate}{Start date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM} for year (YYYY) and month (MM) respectively padded by zeros as needed;}
  \item{edate}{Ending date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM} for year (YYYY) and month (MM) padded by zeros as needed;}
  \item{flowlo}{Optional lower streamflow threshold on which to convert to \code{NA} (to keep continuous day time stamps) through \eqn{Q >} \code{flowlo} \eqn{\rightarrow} \code{NA};}
  \item{flowhi}{Optional upper streamflow threshold on which to convert to \code{NA} (to keep continuous day time stamps) through \eqn{Q <} \code{flowhi} \eqn{\rightarrow} \code{NA};}
  \item{silent}{Suppress informative calls to \code{message};}
  \item{drsilent}{The argument though converted to \dQuote{silent} that is passed to \code{try} for a level of informative error trapping on top of \code{dataRetrieval::}\code{readNWISstat()};}
  \item{drget}{If set, the retrieval from \code{dataRetrieval::}\code{readNWISstat()} is immediately returned following the internal call to \code{dataRetrieval::}\code{renameNWISColumns()};}
  \item{pCode}{Parameter code (default is discharge [streamflow]) and nomenclature almost matches that of the \pkg{dataRetrieval} package;}
  \item{message}{An optional string that if populated will trigger a \code{message()} call that a user might find useful in massive batch processing operations; and}
  \item{...}{Additional arguments to pass to function \code{dataRetrieval::}\code{readNWISstat()}.}
}
\value{
  An \R \code{data.frame} is returned with these expected columns. Some streamgages can have multiple discharges descriptors typed as monthly values---Is this actually true for monthly flows? Only the \code{Flow} column is handled by \pkg{akqdecay}.
  \item{agency_cd}{The agency code for the data;}
  \item{site_no}{The streamgage identification number;}
  \item{site}{The streamgage identification number;}
  \item{year}{The calendar year of the date;}
  \item{month}{The month;}
  \item{decade}{The decade of the daily value. The decade is assign by taking the year and the trailing digit has been stripped and replaced with zero. This is \emph{not} a technique in which a \dQuote{decade} is centered on an even step of 10---meaning, say that 1996--2005 \emph{is not the} \dQuote{2000 decade} but simply 01/*/2000--12/*/2009 \emph{is the} \dQuote{2000 decade}; and}
  \item{Flow}{The streamflow in cubic feet per second, note that the capitalization is from the \pkg{dataRetrieval} package, elsewhere in \pkg{akqdecay} this will become lower case or an alternative name for streamflow.}
}
\note{
For the greater purposes of the \pkg{akqdecay} package, the argument \code{pCode} for the defaults is expected to be left untouched. The capitalization inconsistency in the returned \R \code{data.frame} is left intact as it is consistent with the operation of the \code{dataRetrieval::}\code{renameNWISColumns()} function that is called internally. But at the time of this development (July 2018), the renaming has no effect.
}
\author{ W.H. Asquith}
\references{
Hirsch, R.M., and De Cicco, L.A., 2015, User guide to Exploration and Graphics for RivEr Trends (EGRET) and dataRetrieval: R packages for hydrologic data (version 2.0, February 2015): U.S. Geological Survey Techniques and Methods book 4, chap. A10, 93 p., \url{https://doi.org/10.3133/tm4A10}.
}
\seealso{\code{\link{fill_mnenv}}
}
\examples{
\dontrun{
# USGS 14362000 Applegate River near Copper, Oregon
Copper <- mnget("14362000", sdate="1940-01", edate="1942-06")
print(Copper) # An inspection of the retrieved monthly values. #}
}
\keyword{data retrieval}
\keyword{monthly streamflow}
