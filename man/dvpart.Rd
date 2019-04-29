\name{dvpart}
\alias{dvpart}
\title{ Retrieve USGS Discharge Daily Values structured for Asquith--Knight Discharge Decay Analyses }
\description{
Perform streamflow partitioning to baseflow using port of algorithms of the \pkg{DVstats} package and the function \code{DVstats::part} of daily mean-streamflow values based on the daily value structure of \code{\link{dvget}}. The algorithms implement the procedures of Rutledge (1998). \bold{For the \pkg{akqdecay} package, some new development needs to be made on how to handle missing flows. Instead of issuing \code{stop} as in the \code{part}, the \code{dvpart} implementation returns \code{NULL}. This will help in data mining studies. Need to design a ``lift the pen'' strategy to the \code{NAs} before worrying about \code{fillenv}-like logic.}
}
\usage{
dvpart(akdvtable, sdate="", edate="", cda=NULL, site_no=NA, fillgaps=FALSE, ...)
}
\arguments{
  \item{akdvtable}{A USGS daily-value of streamflow table with some extensions unique to this package as returned by \code{\link{dvget}};}
  \item{sdate}{Start date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed;}
  \item{edate}{Ending date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed;}
  \item{cda}{Contributing drainage area of the streamgage in square miles;}
  \item{site_no}{Optional site number that will be taken if \code{NA} from the \code{akdvtable} as needed;}
  \item{fillgaps}{A logical to trigger the record gap infiller within \code{\link{dvpart}} to log-linearly prorate raw streamflow (\code{Flow}) across record gaps, then perform streamflow separation, and then pack the separated streamflows back onto the input that still retains gaps in record. This gap filler does not actually then appear to create synthetic flow record---this is a very important point; and}
  \item{...}{Other arguments to pass.}
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
  \item{FlowBase}{The baseflow of the \code{Flow} and same as column \code{BaseQ} from \code{DVstats::part};}
  \item{FlowPart1}{The baseflow of the \code{Flow} and same as column \code{Part1} from \code{DVstats::part};}
  \item{FlowPart2}{The baseflow of the \code{Flow} and same as column \code{Part2} from \code{DVstats::part}; and}
  \item{FlowPart3}{The baseflow of the \code{Flow} and same as column \code{Part3} from \code{DVstats::part}.}
}
\note{
For the greater purposes of the \pkg{akqdecay} package, it was desired to have an independent subsystem for streamflow partitioning from external packages. The \code{DVstats::part} as stated above is the basis with some code cleaning to the style of Asquith and Knight that includes the critical adjustment to processing on daily values in the canonical form of \code{\link{dvget}}. The \code{part} implementation requires a short suite of utilities: \code{smwrBase::eventNum}, \code{smwrBase::na2miss}, and \code{smwrBase::shiftData}. These are embedded within \code{dvpart}. Both \code{na2miss} and \code{shiftData} in original form have special handling for factor variables. This logic is not needed and stripped within the \code{dvpart} sources.
}
\author{ W.H. Asquith}
\references{
De Cicco, L., Lorenz, D., 2017, smwrBase---Functions to import and manipulate hydrologic data: R package version 1.1.5, September 18, 2017, accessed April 28, 2019 at \url{https://github.com/USGS-R/smwrBase}.

Lorenz, D., De Cicco, L., 2017, DVstats---Functions to manipulate daily-values data: R package version 0.3.4, July 26, 2017, accessed April 28, 2019 at \url{https://github.com/USGS-R/DVstats}.

Rutledge, A.T., 1998, Computer programs for describing the recession of ground-water discharge and for estimating mean ground-water recharge and discharge from streamflow data--Update: U.S. Geological Survey Water-Resources Investigations Report 98--4148, 43 p., \url{https://pubs.usgs.gov/wri/wri984148/}.
}
\seealso{\code{\link{dvget}}
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
