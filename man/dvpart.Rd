\name{dvpart}
\alias{dvpart}
\title{ Retrieve USGS Discharge Daily Values structured for Asquith--Knight Discharge Decay Analyses }
\description{
Perform streamflow partitioning to baseflow using port of algorithms of the \pkg{DVstats} package and the function \code{DVstats::}\code{part()} of daily mean-streamflow values based on the daily value structure of \code{\link{dvget}}. The algorithms implement the procedures of Rutledge (1998). For the \pkg{akqdecay} package, some new development was made on how to handle missing flows. Instead of issuing \code{stop} as in \code{DVstats::}\code{part()}, the \code{dvpart} implementation returns \code{NULL}. This will help in data mining studies. A more sophisticated method is optionally available (see \bold{Details}).
}
\usage{
dvpart(akdvtable, sdate="", edate="", cda=NULL, site_no=NA, fillgaps=FALSE, ...)
}
\arguments{
  \item{akdvtable}{A USGS daily-value of streamflow table with some extensions unique to this package as returned by \code{\link{dvget}};}
  \item{sdate}{Optional start date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed. The \cr \code{akdvtable} is consulted for the first day of record for the default is \code{sdate = ""};}
  \item{edate}{Optional ending date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed. The \cr \code{akdvtable} is consulted for the last day of record for the default is \code{edate = ""};}
  \item{cda}{Contributing drainage area of the streamgage in square miles;}
  \item{site_no}{Optional site number that will be taken if \code{NA} from the \code{akdvtable} as needed;}
  \item{fillgaps}{A logical to trigger the record gap infiller; and}
  \item{...}{Other arguments to pass.}
}
\details{
Missing data and gaps in the record can actually be accommodated by a simple approach triggered by the \code{fillgaps} argument. It does require mentioning that there are two types of missingness. Real \code{NA}s could be delivered on the daily value retrieval, but also jumps or gaps in time too. The \code{dvpart} checks for both conditions and returns \code{NULL} if either is encountered---Except if the \code{fillgaps} is set.

The strategy has the following steps. First, construct an absolutely daily-continuous sequence of data from \code{sdate} to \code{edate}. Second, flows that are less than or equal to zero are set to small number (see \code{pmax} use in the sources). Third, log-linearly interpolate for the daily-continuous sequence that streamflow across record gaps using the non-\code{NA} flows and respective dates in \code{akdvtable}. Fourth, streamflow separation is made on the daily-continuous streamflows.

The fifth step is critical. For only the dates in the \code{akdvtable}, extract the streamflow separation estimates and add them to the table. This means that the output table from this function still retains gaps in record.
}
\value{
  An \R \code{data.frame} is returned with these expected columns.
  \item{agency_cd}{The agency code for the data;}
  \item{site_no}{The streamgage identification number;}
  \item{Date}{The date, note that the capitalization is from the \pkg{dataRetrieval} package, elsewhere in \pkg{akqdecay} this will become lower case;}
  \item{Flow}{The streamflow in cubic feet per second, note that the capitalization is from the \pkg{dataRetrieval} package, elsewhere in \pkg{akqdecay} this will become lower case or an alternative name for streamflow;}
  \item{Flow_cd}{A coding system for the streamflow (\code{A}, approved record; \code{P}, provisional, and \code{W}, working record);}
  \item{year}{The calendar year of the date;}
  \item{decade}{The decade of the daily value. The decade is assign by taking the year and the trailing digit has been stripped and replaced with zero. This is \emph{not} a technique in which a \dQuote{decade} is centered on an even step of 10---meaning, say that 1996--2005 \emph{is not the} \dQuote{2000 decade} but simply 01/01/2000--12/31/2009 \emph{is the} \dQuote{2000 decade;}}
  \item{wyear}{The water year;}
  \item{month}{The month.}
  \item{FlowBase}{The baseflow of the \code{Flow} (the column \code{BaseQ} from \code{DVstats::}\code{part()});}
  \item{FlowPart1}{The baseflow of the \code{Flow} (the column \code{Part1} from \code{DVstats::}\code{part()});}
  \item{FlowPart2}{The baseflow of the \code{Flow} (the column \code{Part2} from \code{DVstats::}\code{part()}); and}
  \item{FlowPart3}{The baseflow of the \code{Flow} (the column \code{Part3} from \code{DVstats::}\code{part()}).}
}
\note{
For the greater purposes of the \pkg{akqdecay} package, it was desired to have an independent subsystem for streamflow partitioning from external packages. The \code{DVstats::}\code{part()} as stated above is the basis with some code cleaning to the style of Asquith and Knight that includes the critical adjustment to processing on daily values in the canonical form of \code{\link{dvget}}. The \code{part()} implementation requires a short suite of utilities: \code{smwrBase::}\code{eventNum()}, \code{smwrBase::}\code{na2miss()}, and \code{smwrBase::}\code{shiftData()}. These are embedded within \code{dvpart}. Both \code{na2miss()} and \code{shiftData()} in original form have special handling for factor variables. This logic is not needed and stripped within the \code{dvpart} sources.
}
\author{ W.H. Asquith}
\references{
De Cicco, L., Lorenz, D., 2017, smwrBase---Functions to import and manipulate hydrologic data: R package version 1.1.5, September 18, 2017, accessed April 28, 2019 at \url{https://github.com/USGS-R/smwrBase}.

Lorenz, D., De Cicco, L., 2017, DVstats---Functions to manipulate daily-values data: R package version 0.3.4, July 26, 2017, accessed April 28, 2019 at \url{https://github.com/USGS-R/DVstats}.

Rutledge, A.T., 1998, Computer programs for describing the recession of ground-water discharge and for estimating mean ground-water recharge and discharge from streamflow data--Update: U.S. Geological Survey Water-Resources Investigations Report 98--4148, 43 p., \url{https://pubs.usgs.gov/wri/wri984148/}.
}
\seealso{\code{\link{dvget}}, \code{\link{fill_dvpartenv}}
}
\examples{
\dontrun{
# USGS 14362000 Applegate River near Copper, Oregon
dv <- dvget("14362000", sdate="1989-10-01", edate="1999-09-30")
pdv <- dvpart(dv, cda=225) # The drainage area is 225 square miles.
plot(pdv$Date,  pdv$Flow, log="y", type="l", col=8)
lines(pdv$Date, pdv$FlowBase,  col=1)
lines(pdv$Date, pdv$FlowPart1, col=2)
lines(pdv$Date, pdv$FlowPart2, col=3)
lines(pdv$Date, pdv$FlowPart3, col=4) #}
}
\keyword{streamflow partitioning}
\keyword{baseflow}
