\encoding{utf8}
\name{akq_summaryfilter}
\alias{akq_summaryfilter}
\title{Filter the Summary Table from Asquith--Knight Discharge Decay Analyses}
\description{
Apply a filter to the summary table from Asquith--Knight discharge decay analyses to get alternative lists of site numbers by processing the summary tables returned by \code{\link{akqdecay}} for a sequence of USGS streamgages contained within the \R \code{environment} that has been previously populated by \code{\link{fill_akqenv}}.  The filter is built around only one filter operation per pass. Thus, if a combination of say \code{begyr} and \code{endyr} is desired, users will have to build a post-processing \code{intersect()} operation themselves on the returned sites.
}
\usage{
akq_summaryfilter(akqenv, mindv=NA, begyr=NA, endyr=NA, silent=FALSE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} populated by \code{\link{fill_akqenv}}. This argument can also be an \R \code{list} directly from \code{\link{akqdecay}}; special accommodation is made;}
  \item{mindv}{The minimum number of daily values to compare to the \code{total_count};}
  \item{begyr}{The minimum (beginning) year to compare to the \code{beg_year};}
  \item{endyr}{The maximum (ending) year to compare to the \code{end_year};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An vector for the \code{site} values passing the filter.
}
\author{ W.H. Asquith}
\seealso{\code{\link{akq_summary}}
}
\examples{
\dontrun{
sites <- c("01396660", "01400500") # Two USGS streamgages in New Jersey
njdv  <- new.env(); fill_dvenv(sites, envir=njdv)  # 01396660 first year 1977
njAKQ <- new.env(); fill_akqenv(dvenv=njdv, envir=njAKQ) # 01400500 first year 1903
print(akq_summaryfilter(njAKQ, mindv=18250))  # only 01400500 is returned}
}
\keyword{data extraction}
