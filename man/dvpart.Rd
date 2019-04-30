\name{dvpart}
\alias{dvpart}
\title{ Peform Streamflow Partitioning after Rutledge (1998) Structured for Processing the Asquith--Knight Daily Streamflow Table }
\description{
Perform streamflow partitioning to baseflow using port of algorithms of the \pkg{DVstats} package and the function \code{DVstats::}\code{part()} of daily mean-streamflow values based on the daily value structure of \code{\link{dvget}}. The function \code{\link{fill_dvpartenv}} is a wrapper that can be used to fill an \R \code{environment} with the output of the \code{dvpart} function.


The algorithms implement the procedures of Rutledge (1998). For the \pkg{akqdecay} package, some new development was made on how to handle missing flows to help in data mining studies. First, instead of issuing \code{stop()} as done in \code{DVstats::}\code{part()}, the \code{dvpart} implementation returns \code{NULL}. Second, a more sophisticated method is optionally available (see \bold{Details}).
}
\usage{
dvpart(akdvtable, sdate="", edate="", cda=NA, site_no=NA, fillgaps=FALSE, ...)
}
\arguments{
  \item{akdvtable}{A USGS daily-value of streamflow table with some extensions unique to this package as returned by \code{\link{dvget}};}
  \item{sdate}{Optional start date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed. The \cr \code{akdvtable} is consulted for the first day of record for the default is \code{sdate = ""};}
  \item{edate}{Optional ending date (default is earliest) and nomenclature matches that of the \pkg{dataRetrieval} package with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed. The \cr \code{akdvtable} is consulted for the last day of record for the default is \code{edate = ""};}
  \item{cda}{Contributing drainage area of the streamgage in square miles. Only the first value is used without warning if a vector is given. The area can readily be determined from \code{\link{sites_to_SpatialPointsDataFrame}}, though a missing area will default to unity inside \code{dvpart};}
  \item{site_no}{Optional site number that will be taken if \code{NA} from the \code{akdvtable} as needed;}
  \item{fillgaps}{A logical to trigger the record gap infiller; and}
  \item{...}{Other arguments to pass.}
}
\details{
Missing data and gaps in the record can actually be accommodated by a simple approach triggered by the \code{fillgaps} argument. It does require mentioning that there are two types of missingness. Real \code{NA}s could be delivered on the daily value retrieval, but also jumps or gaps in time too. The \code{dvpart} checks for both conditions and returns \code{NULL} if either is encountered---Except if the \code{fillgaps} is set.

The strategy has the following steps. First, construct an absolutely daily-continuous sequence of dates from \code{sdate} to \code{edate}. Second, flows that are less than or equal to zero are set to small number (see \code{pmax} use in the sources). Third, log-linearly interpolate for the daily-continuous sequence that streamflow across record gaps using the non-\code{NA} flows and respective dates in \code{akdvtable}. Fourth, streamflow separation is made on the daily-continuous streamflows instead of the non-\code{NA} original non-daily continuous data.

The fifth and last step is crucial. For only the dates in the \code{akdvtable}, extract the streamflow separation estimates and add them to the table. This means that the output table from this function still retains gaps in record.
}
\value{
  An \R \code{data.frame} is returned with these expected columns.
  \item{agency_cd}{The agency code for the data;}
  \item{site_no}{The streamgage identification number;}
  \item{Date}{The date, note that the capitalization is from the \pkg{dataRetrieval} package, elsewhere in \pkg{akqdecay} this will become lower case;}
  \item{Flow}{The streamflow in cubic feet per second, note that the capitalization is from the \pkg{dataRetrieval} package, elsewhere in \pkg{akqdecay} this will become lower case or an alternative name for streamflow;}
  \item{Flow_cd}{A coding system for the streamflow (\code{A}, approved record; \code{P}, provisional, and \code{W}, working record);}
  \item{site}{A character representation of the \code{site_no}, which likely is already a character. The reasoning for another site column is in case a user need some type of flexibility in later processing. The user could freely replace contents of this column;}
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
For the greater purposes of the \pkg{akqdecay} package, it was desired to have an independent subsystem for streamflow partitioning from external packages. The \code{DVstats::}\code{part()} as stated above is the basis with some code cleaning to the style of Asquith and Knight that includes the critical adjustment to processing on daily values in the canonical form of \code{\link{dvget}}. The \code{part()} implementation requires a short suite of utilities: \code{smwrBase::}\code{eventNum()}, \code{smwrBase::}\code{na2miss()}, and \code{smwrBase::}\code{shiftData()}. These are embedded within \code{dvpart}. Both \code{na2miss()} and \code{shiftData()} in original form have special handling for factor variables. This logic is not needed and has been stripped within the \code{dvpart} sources.

Rutledge (1998) is the basis for the algorithm. One oft question is why a drainage area is needed for the streamflow partitioning. Rutledge (1998, p. 10) states ``The duration of time after a peak in streamflow during which the components of streamflow due to surface runoff and interflow are significant can be estimated [by empirical formula].'' The formula is \eqn{N = A^{0.2}}, where \eqn{N} is the number of days after the peak and \eqn{A} is the drainage area in square miles. Algorithmically, the \eqn{N} is then cast as the integer \eqn{\lceil N \rceil} using the \code{ceiling()} function in \R. Rutledge (1998, p. 3) also states that the lower limit is 1 square mile and suggests 500 square miles as an upper limit (2,000 square miles is also mentioned as an upper limit). No check against these limits is provided in \code{dvpart} and evidently not in \code{DVstats::}\code{part()} for that matter.
}
\author{ W.H. Asquith}
\references{
De Cicco, L.A., Lorenz, D.L., 2017, smwrBase---Functions to import and manipulate hydrologic data: R package version 1.1.5, September 18, 2017, accessed April 28, 2019 at \url{https://github.com/USGS-R/smwrBase}.

Lorenz, D.L., De Cicco, L.A., 2017, DVstats---Functions to manipulate daily-values data: R package version 0.3.4, July 26, 2017, accessed April 28, 2019 at \url{https://github.com/USGS-R/DVstats}.

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

\dontrun{
DV  <- dvget("07031740", edate="2017-12-31"); cda <- 788 # square miles
pDV <- dvpart(DV, cda=cda) # NULL will be return and here is the message
#         for 07031740 noncontinuous data between 1995-02-01 to 2017-11-28
#         gaps about: 1995-03-23[12days], 1995-06-25[53days], 1995-10-22[22days],
# 1996-02-12[18days], 1996-03-19[11days], 1996-04-20[26days], 1996-06-29[65days],
# 2017-01-21[97days], 2017-03-07[28days], 2017-04-05[17days], 2017-04-27[6days],
# 2017-06-16[3days] : total=358

pDV <- dvpart(DV, cda=cda, fillgaps=TRUE) # see Details
length(pDV$Date)  # 7,991. This means this many discharges NA or numeric
length(seq(pDV$Date[1],
       as.Date("2017-12-31"), by=1)) # 8370. This mean days desired/expected.
# Note 8370 - 7991 != 358 above because gaps at beginning or ending not counted.

DV  <- dvget("07031740", edate="2017-12-31", ignore.provisional=FALSE)
pDV <- dvpart(DV, cda=cda, fillgaps=FALSE)
#          for 07031740 noncontinuous data between 1995-02-01 to 2017-12-31
#          gaps about: 1995-03-23[12days], 1995-06-25[53days], 1995-10-22[22days],
# 1996-02-12[18days], 1996-03-19[11days], 1996-04-20[26days],
# 1996-06-29[65days] : total=207
# ** So we see improvement! Gaps only at beginning of gage. ** #}

\dontrun{
# Let us now work on practical example with further implications.
# We want monthly baseflows for this gage that had the record gaps.
cda <- sites_to_SpatialPointsDataFrame("07031740")$CDA # 788 square miles
DV  <- dvget("07031740", edate="2017-12-31", ignore.provisional=FALSE)
pDV <- dvpart(DV, cda=cda, fillgaps=TRUE)
length(DV$Date)  # 8,170   See how no new days are created!
length(pDV$Date) # 8,170   This is a big deal (see Details).

# now compute monthly mean values of the flows, number of observations
# suppressWarnings() used because mean of character columns is a warning
suppressWarnings(MN <- aggregate(pDV, by=list(pDV$year, pDV$month), mean))
ml <- data.frame(year=pDV$year, month=pDV$month) # subset for speed
suppressWarnings(ml <- aggregate(ml, by=list(ml$year, ml$month), length))

# tidy up the results, resort to year, month ordering
MN <- MN[order(MN$Group.1),]; ml <- ml[order(ml$Group.1),];
MN$Group.1 <- MN$Group.2 <- NULL # delete these two columns
MN$agency_cd <- pDV$agency_cd[1]  # add back the agency code
MN$site_no   <- pDV$site_no[1]    # add back the streamgage number
MN$month_count   <- ml$month; rm(ml)  # delete the ml (do not need further)
MN$days_in_month <- lubridate::days_in_month(MN$Date)
MN <- MN[MN$month_count/MN$days_in_month == 1,]
nm <- names(MN); nm[3] <- "DateMean"; names(MN) <- nm; rm(nm)
tail(MN); # shows July 2017 Flow as 351.3871, manually NWISWeb shows 351.4
          # shows July 2017 FlowBase as 275.7239
plot(MN$DateMean, MN$Flow, type="l", log="y",
                  xlab="Date", ylab="Streamflow, cfs")
lines(MN$DateMean, MN$FlowBase, col=2) #}
}
\keyword{streamflow partitioning}
\keyword{baseflow}
