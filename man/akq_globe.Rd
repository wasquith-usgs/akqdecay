\encoding{utf8}
\name{akq_globe}
\alias{akq_globe}
\title{Global Summary of Count Tables from the Asquith--Knight Discharge Decay Analyses}
\description{
Produce global summary of the count tables from Asquith--Knight discharge decay analyses returned by \code{\link{akqdecay}} for a sequence of USGS streamgage identification numbers contained within an \R \code{environment}. This environment has already been populated by \code{\link{fill_akqenv}}. The counts of number of sites, total (cumulative record length of daily values [minus the \code{lag} used for \code{\link{akqdecay}}?]) and totals for increasing, decreasing, no change, and missing days are computed.
}
\usage{
akq_globe(akqenv, silent=TRUE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} previously populated by \code{\link{fill_akqenv}}. This argument can also be an \R \code{list} directly from \code{\link{akqdecay}}; special accommodation is made;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{list} containing the following for all of the streamgages.
  \item{counts}{The number sites plus totals of the various counts (integers); and}
  \item{percents}{The conversion of the counts to percent where \code{total_count} is treated as the sum total.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{akq_counts}}
}
\examples{
\dontrun{
sites <- c("05403500", "05405000") # Two USGS streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,              envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
akq_globe(WisAKQ)
#$counts
# num_sites total_count decreases increases no_change NAs
#         2       43280     22114     15426      5731   9
#$percents
# num_sites pct_total_count pct_decreases pct_increase pct_nochange pct_NAs
#         2             100      51.09519     35.64233     13.24168 0.02079 #}
}
\keyword{data extraction}
\keyword{data summary}
