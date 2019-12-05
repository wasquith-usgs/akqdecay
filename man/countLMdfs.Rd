\encoding{utf8}
\name{countLMdfs}
\alias{countLMdfs}
\title{Counts and Percentages from L-moment Tables of the Asquith--Knight Discharge Decay Analyses}
\description{
Extract and combine the counts from various yearly or decadal L-moment tables between the three computation methods of Asquith--Knight discharge decay analyses and compute relative percentages. A check is made internally that the ordering of the arguments \code{DECR}, \code{INCR}, and \code{NC} looks proper---meaning that total summation (all sites and year/decade) of \eqn{\check\Psi} (decreasing) are larger than total summation of \eqn{\hat\Psi} and larger than total summation of \eqn{\ddot\Psi}, and total summation of \eqn{\hat\Psi} is larger than total summation of \eqn{\ddot\Psi}. This check is made because the number of decreases is almost uniformly about twice as often as increases and both those are larger than the number of no changes. Mathematically, for indicator function \eqn{\mathbf{1}(x_i)} for the presence of the \eqn{i}th value in the sample, the relations among the \eqn{\Psi} are
\deqn{\sum_{i=1}^n \mathbf{1}(\check\Psi_i) \ge \sum_{i=1}^n \mathbf{1}(\hat\Psi_i) \ge \sum_{i=1}^n \mathbf{1}(\ddot\Psi_i)\mbox{.}}
}
\usage{
countLMdfs(DECR, INCR, NC, silent=FALSE, ...)
}
\arguments{
  \item{DECR}{A \code{decreasing} (the \code{method} call on \code{\link{akqdecay}}) L-moment table (year or decade) from \code{\link{akq_lmom_yeardecade}};}
  \item{INCR}{An \code{increasing} (the \code{method} call on \code{\link{akqdecay}}) L-moment table (year or decade) from \code{\link{akq_lmom_yeardecade}};}
  \item{NC}{An \code{nochange} (the \code{method} call on \code{\link{akqdecay}}) L-moment table (year or decade) from \code{\link{akq_lmom_yeardecade}};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{data.frame} is returned having attributes (\code{attr()} function) of \code{skipped_sites} and \code{which} (year or decade) and contains
  \item{site}{The streamgage identification number;}
  \item{year/decade}{The type of aggregation;}
 \item{total_count_minusNAs}{The total number of daily-mean streamflow values. However, the counts of the \code{NAs} that are reported for the period of record by \code{\link{akq_counts}} not not available (This might be a TODO for the package). This total is \eqn{\sum [\mathbf{1}(\check\Psi)  + \mathbf{1}(\hat\Psi) + \mathbf{1}(\ddot\Psi)]};}
  \item{decreases}{The number of decreases (\eqn{\check\Psi}) during the year or decade;}
  \item{increases}{The number of increases (\eqn{\hat\Psi}) during the year or decade;}
  \item{nochanges}{The number of no changes (\eqn{\ddot\Psi}) during the year or decade;}
  \item{pct_decreases}{Percent of decreases relative to \code{total_count_minusNAs};}
  \item{pct_increases}{Percent of increases relative to \code{total_count_minusNAs}; and}
  \item{pct_nochanges}{Percent of no changes relative to \code{total_count_minusNAs}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{akq_lmom_year}}, \code{\link{akq_lmom_decade}}
}
\examples{
\dontrun{
# Suppose LDd [decreasing], LDi [increasing], LDnc [nochange] are from
# LDd <- akq_lmom_decade(AKd); LDi <- akq_lmom_decade(AKi); LDnc <- akq_lmom_decade(AKnc)
# for which AKd, AKi, and AKnc are from fill_akqenv(..., method="decreasing") etc...
DIN <- countLMdfs(LDd, LDi, LDnc) # *** Here is the example operation. ***
# Here follows the function's response via message() during some testing.
#countLMdfs: remember that counts of NAs (record gaps or zero flows) are not included.
#    Totals: decreases=10971798, increases=5627551, and nochanges=1976068 and data.frame
#    argument ordering seems consistent.
par(las=1, xaxs="i", yaxs="i")
plot(jitter(DIN$decade, factor=2), DIN$pct_decreases, cex=0.6, tcl=0.5,
     xlim=c(1880,2020), ylim=c(0,100), col="#FB6A4A", lwd=0.5,
     xlab="DECADE", ylab="FRACTION OF TIME")
points(jitter(DIN$decade, factor=2), DIN$pct_increases, cex=0.6, col="#6BAED6", lwd=0.5)
points(jitter(DIN$decade, factor=2), DIN$pct_nochanges, cex=0.6, col="#74C476", lwd=0.5)
tmp <- DIN[complete.cases(DIN),]
lines(lowess(tmp$decade, tmp$pct_decreases), col="#A50F15", lwd=2)
lines(lowess(tmp$decade, tmp$pct_increases), col="#08519C", lwd=2)
lines(lowess(tmp$decade, tmp$pct_nochanges), col="#006D2C", lwd=2)
# This example uses jitter() because likey enormous overlap of points for a given
# decade will occur. The lowess() lines are drawn to help visualize the pattern.
# A common pattern seen in exploratory analysis is a systematic decrease in no changes
# from the earliest days of the USGS streamgaging to modern times. Ramifications of this
# are not fully understood (noted March 30, 2017, wha/rrk).}
}
\keyword{data extraction}
\keyword{L-moments}
