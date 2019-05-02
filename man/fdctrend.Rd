\name{fdctrend}
\alias{fdctrend}
\title{ Perform Kendall's Tau Test for Monotonic Trend by Year for each of 365 Quantiles of the Flow-Duration Curve  }
\description{
The \code{cor.test} function built-in to \R computes the Kendall's tau test between two variables, and if one variable is time, then the test is one for monotonic correlation (association) with time (Helsel and Hirsch, 2002).  An annual daily-flow-duration curve (empirical distribution function of the daily mean streamflow) is constructed for each year, and for this function, a year is calendar and not water year. Hirsch refers to this analysis type as \dQuote{Quantile-Kendall.}

Not counting leap years, there are 365 days in a year. From the annual daily-flow-duration curve and for each of the 365 probabilities (actually plotting position form determined by the \code{type} argument), the streamflow is computed. Then for each probability, the correlation test is applied for the available years. The estimated correlation coefficient and its p-value are computed. There is no feature for testing of statistical significance, that is left to the end user. Finally, the probabilities \eqn{F} by type (\eqn{t \in [4,9]}) for a ranking of \eqn{d \in [1,n]} for \eqn{n = 365} by
\deqn{
  F =  d       /n \quad \mathrm{for}\ t = 4\mbox{,}
}
\deqn{
  F = (d - 0.5)/n \quad \mathrm{for}\ t = 5\mbox{,}
}
\deqn{
  F =  d       /(n + 1) \quad \mathrm{for}\ t = 6\mbox{,}
}
\deqn{
  F = (d - 1)  /(n - 1) \quad \mathrm{for}\ t = 7\mbox{,}
}
\deqn{
  F = (d - 1/3)/(n - 1/3) \quad \mathrm{for}\ t = 8\mbox{, and}
}
\deqn{
  F = (d - 3/8)/(n + 1/4) \quad \mathrm{for}\ t = 9\mbox{.}
}
}
\usage{
fdctrend(akdvtable, missing.days=7, type=5, ...)
}
\arguments{
  \item{akdvtable}{A USGS daily-value of streamflow table with some extensions unique to this package as returned by \code{\link{dvget}};}
  \item{missing.days}{The number of permissible missing days in a given year on which to still compute the annual daily-flow-duration curve and use that year in the trend test; The default is one week and it is advised to not exceed that count. The streamflow values are first reduced by removing \code{NA} before comparing the total number of values for a year against the value 365 minus \code{missing.days}. The call to \code{quantile} will fail if \code{NA}s are present and the developer deliberately has this and does not toggle the \code{na.rm} of the \code{quantile} function;}
  \item{type}{The \code{type} argument of the \code{quantile()} function built-in to \R; and}
  \item{...}{Additional arguments to pass to control the \code{cor.test()} function built-in to \R.}
}
\value{
An \R \code{data.frame} of mutable width. The first column is a \dQuote{day} (just a counter) from 1 to 365. The \code{row.names} are the probabilities as determined by \code{type} and labeled as character strings from the \code{quantile()} function. Then successively columns towards the right are the flow-duration curves for each year where the column name is the year. The third from last column is the probability in numeric form. The second from last column is the estimated correlation coefficient. Finally, the last column is the computed p-value.  The \code{warnings()} are turned off when calling the \code{cor.test()} so that we are not warned of ties in the data affecting the accuracy of the p-value as ties are endemic in daily streamflow data sets.
}
\author{ W.H. Asquith}
\references{
Helsel, D.R. and Hirsch, R.M., 2002, Statistical Methods in Water Resources: U.S. Geological Survey Techniques of Water Resources Investigations, book 4, chapter A3, 522 p., \url{https://pubs.usgs.gov/twri/twri4a3/}
}
\seealso{\code{\link{fill_tfdcenv}}
}
\examples{
# See also Examples under fill_tfdcenv and elsewhere in this documentation.
\dontrun{
dv <- dvget("08167000", edate="2015-09-30", ignore.provisional=FALSE)
ft <- fdctrend(dv); head(ft) # see that the 2015 year is gone because incomplete
qk <- visFDCtrend(ft); # qk stores ordinates of the Quantile-Kendall plot #}
}
\keyword{flow-duration curve}
\keyword{flow-duration curve (trend)}
\keyword{Kendall tau}
