\name{dvpart2mnpart}
\alias{dvpart2mnpart}
\title{ Perform Streamflow Partitioning after Rutledge (1998) Structured for Processing the Asquith--Knight Daily Streamflow Table }
\description{
Convert daily streamflow partitioning from the \R \code{data.frame} from \code{\link{dvpart}} to monthly streamflow partitioning. Only complete months are permitted, which means (say) that 30 days of streamflows (baseflow and others) in an October will not be converted to a monthly values.
}
\usage{
dvpart2mnpart(pdv, ...)
}
\arguments{
  \item{pdv}{The value from \code{\link{dvpart}}; and}
  \item{...}{Other arguments to pass.}
}
\value{
  An \R \code{data.frame} is returned with these expected columns. Note that the requirement for a complete month means some months might not be presented and that the \code{DateMean} is not required to be monthly continuous.
  \item{agency_cd}{The agency code for the data;}
  \item{site_no}{The streamgage identification number;}
  \item{Date}{The date set to the first of the month. This presentation might be useful if this table were to be joined against other tables, such as meteorologic that use the first of the month to represent time;}
  \item{DateMean}{The mean date, which wil be the 14th, 15th, or 16th depending on the month and year. This is a diagnostic on the mean aggregation by the \code{aggregate()} function in the sources;}
  \item{Flow}{The monthly streamflow in cubic feet per second;}
  \item{site}{A character representation of the \code{site_no}, which likely is already a character. The reasoning for another site column is in case a user need some type of flexibility in later processing. The user could freely replace contents of this column;}
  \item{year}{The calendar year of the date;}
  \item{decade}{The decade of the daily value. The decade is assign by taking the year and the trailing digit has been stripped and replaced with zero. This is \emph{not} a technique in which a \dQuote{decade} is centered on an even step of 10---meaning, say that 1996--2005 \emph{is not the} \dQuote{2000 decade} but simply 01/01/2000--12/31/2009 \emph{is the} \dQuote{2000 decade;}}
  \item{wyear}{The water year;}
  \item{month}{The month.}
  \item{FlowBase}{The monthly baseflow of the \code{Flow};}
  \item{FlowPart1}{The monthly partition 1 of baseflow;}
  \item{FlowPart2}{The monthly partition 2 of baseflow;}
  \item{FlowPart3}{The monthly partition 3 of baseflow;}
  \item{month_count}{The number of daily observations in the month that is dependent on the actually number of day of streamflow represented and not the real number of days in the month; and}
  \item{days_in_month}{The real number of days in the month. This column is equal to \code{month_count} when the criteria that a complete month of streamflow must be available.}
}
\note{
The code here represents monthly mean computation independent of USGS monthly values that are also reported through the Internet but with rounding by the USGS being used. Nevertheless, the two values are expected to be very close to each other. This means that the \code{\link{mnget}} function of this package is not used for \code{dvpart2mnpart}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{dvget}}, \code{\link{dvpartenv2mnpartenv}}
}
\examples{
\dontrun{
# USGS 14362000 Applegate River near Copper, Oregon
dv <- dvget("14362000", sdate="1989-10-01", edate="1999-09-30")
pdv <- dvpart(dv, cda=225) # The drainage area is 225 square miles.
pmn <- dvpart2mnpart(pdv) # }
}
\keyword{streamflow partitioning}
\keyword{baseflow}
\keyword{monthly streamflow}

