\name{akq_counts}
\alias{akq_counts}
\title{ Count Table from the Asquith--Knight Discharge Decay Analyses }
\description{
Extract the count table (\code{counts}) from Asquith--Knight discharge decay analyses. The count table is an \R \code{data.frame} has been already made by \code{\link{akqdecay}} for a sequence of USGS streamgage identification numbers contained within an \R \code{environment}. This environment has already been populated by \code{\link{fill_akqenv}}. The various counts of direction of change in the daily-mean streamflows are computed (decreasing, increasing, or no change) in \code{\link{akqdecay}} regardless of the arguments \code{decreasing} and \code{lag} originally passed to that function.
}
\usage{
akq_counts(akqenv, silent=FALSE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} previously populated by \code{\link{fill_akqenv}}. This argument can also be a direct \R \code{list} from \code{\link{akqdecay}}; special accommodation is made;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{data.frame} containing the counts for each streamgage.
  \item{site}{The streamgage identification number;}
  \item{total_count}{The total number of daily-mean streamflow values;}
  \item{decreases}{The number of decreases (\eqn{\check\Psi});}
  \item{increases}{The number of increases (\eqn{\hat\Psi});}
  \item{nochanges}{The number of no changes (\eqn{\ddot\Psi});}
  \item{NAs}{The number of missing values. Total count is the sum of the decreases, increases, no changes, and number of \code{NAs}. The \code{NAs} can results from gaps in record, zero flow in the data, and transitions from flow to no flow or no flow back to flow;}
  \item{rawDV_zeros}{This is a counter on the number of zero values \emph{before the differencing} (the original data), hence the term \code{raw}. This value is provided to help a user in interpretation of the \code{NAs};}
  \item{rawDV_NAs}{This is a counter on the number of \code{NA} values \emph{before the differencing} (the original data), hence the term \code{raw}. This value is provided to help a user in interpretation of the \code{NAs}; and}
  \item{delDates_ne_lag}{This is a \dQuote{delta (change) between dates not equal to the lag} counter. Such a counter tracks the number of changes between successive dates that are not equal to the \code{lag} and thus represent data values not involved in statistical computation.}
}
\note{
\bold{What does delta (change) between dates not equal to the lag mean?} Gaps in record should result in a missing value for the \dQuote{tomorrow} date (note the use of the subscripts for date in the text that follows as presenting the tomorrow [forward bias notation]). For example, a transition from \eqn{\mathrm{Jan1}} (January 1st) to \eqn{\mathrm{Jan2}} would have a \eqn{\Delta\mathrm{Date}_\mathrm{Jan2} = 1}, but a transition from \eqn{\mathrm{Jan2}} to \eqn{\mathrm{Jan4}} (because \eqn{\mathrm{Jan3}} is not available, say streamgage not operational or perhaps record flagged as provisional) would have a \eqn{\Delta\mathrm{Date}_\mathrm{Jan4} = 2}. If the \code{lag=1}, then only the former and not the later enter into statistical computation.

\bold{Visualization of count distributions?} Here is a recipe for visualizing the quantile distribution of the total number of \eqn{\Psi} available in \code{akqenv}:
\preformatted{
  CN <- akq_counts(akqenv)
  ylim <- range(CN$total_count, CN$nochanges, CN$increases, CN$decreases)
  ylim[1] <- ifelse(ylim[1] == 0, 1, ylim[1])
  plot(qnorm(lmomco::pp(CN$total_count)),sort(CN$total_count),log="y",lwd=0.8,
       xlab="Standard Normal Variate", ylab="Number of Differences")
  points(qnorm(lmomco::pp(CN$nochanges)), sort(CN$nochanges),  col=3, lwd=0.8)
  points(qnorm(lmomco::pp(CN$increases)), sort(CN$increases),  col=4, lwd=0.8)
  points(qnorm(lmomco::pp(CN$decreases)), sort(CN$decreases),  col=2, lwd=0.8)
  legend(0,100, c("total_count", "nochanges", "increases", "decreases"),
                col=c(1,3,4,2), pch=c(1,1,1,1), bty="n")
}
}
\author{ W.H. Asquith}
\seealso{\code{\link{akqdecay}}, \code{\link{fill_akqenv}}, \code{\link{akq_lmom}}, \code{\link{akq_summary}}
}
\examples{
\dontrun{
sites <- c("05403500", "05405000") # Two USGS streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,              envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
akq_counts(WisAKQ) # hand editing for line wrapping made below
    site total_count decreases increases nochanges NAs
05403500       17714      8821      6011      2873   9
05405000       25566     13293      9415      2858   0
    site rawDV_zeros rawDV_NAs delDates_ne_lag
05403500           0         0               9
05405000           0         0               0 #}
}
\keyword{data extraction}
