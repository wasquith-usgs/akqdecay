\encoding{utf8}
\name{akq_summary}
\alias{akq_summary}
\title{Summary Table from the Asquith--Knight Discharge Decay Analysis}
\description{
Extract the summary table from Asquith--Knight discharge decay analyses returned by \code{\link{akqdecay}} for a sequence of USGS streamgages contained within the \R \code{environment} that has been previously populated by \code{\link{fill_akqenv}}.
}
\usage{
akq_summary(akqenv, silent=FALSE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} populated by \code{\link{fill_akqenv}}. This argument can also be an \R \code{list} directly from \code{\link{akqdecay}}; special accommodation is made;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{data.frame} containing the summary for each streamgage.
\item{site}{The streamgage identification number;}
\item{beg_year}{First year in period of record data retrieval;}
\item{end_year}{Last year in period of record data retrieval;}
\item{yr_range_str}{A neat string representation that might be useful in formal tables for publication;}
\item{total_count}{The total number of daily values;}
\item{count}{The count of daily values processed by the settings of \code{\link{akqdecay}};}
\item{kendall_tau}{Kendall's Tau between the days per log-cycle changes and the streamflow from the flow-duration curve;}
\item{spearman_rho}{Spearman's Rho between the days per log-cycle changes and the streamflow from the flow-duration curve;}
\item{median}{The median (\eqn{\Psi_\mathrm{med}}) of the processed daily values;}
\item{L1L2}{The mean plus square-root pi L-scale: \eqn{\lambda_1 + \lambda_2\sqrt{\pi}} (see \bold{Note});}
\item{gfactor}{The \eqn{G_f}-factor from a fitted distribution for the probability \code{f} used by \cr \code{\link{akqdecay}}; and}
\item{gfactor_emp}{The \eqn{G_f}-factor from the empirical distribution for probability \code{f} through a call to the standard (default) built-in \R \code{quantile()} function.}
}
\note{
As yet, Asquith and Knight have definitive opinion on what the optimal \dQuote{Gfactor} is in regards to depth into the distribution tail of days per log-cycle change on the recession limb of hydrographs. Please contact the authors for further information.

The \code{L1L2} as a term or concept is expected to have little direct meaning to most users. The first L-moment (arithmetic mean) is \eqn{\lambda_1}. The second L-moment (L-scale) is \eqn{\lambda_2} and is directly interpretable as, but not numerically equal to, the well-known standard deviation (\eqn{\sigma}). Thus, it has units of days per log-cycle and matches those of the mean. The \eqn{\sigma} is the second parameter of the normal distribution, and this parameter in terms of L-scale is \eqn{\sigma = \lambda_2\sqrt{\pi}}. Thus, this is an estimate of the standard deviation via L-moments. The percentile \code{pnorm(sqrt(pi), sd=sqrt(pi))} is the 84 percentile of the normal for \eqn{\lambda_1 = 0}---hence, \code{L1L2} can loosely be thought of as a parametric estimate of a \eqn{G_{\mathrm{nor}}(F=0.84)} Gfactor. This would be a Gfactor defined as about \dQuote{one standard deviation into the right tail of the distribution.}

}
\author{ W.H. Asquith}
\seealso{\code{\link{akqdecay}}, \code{\link{akq_lmom}}, \code{\link{akq_counts}}, \code{\link{akq_summary}}
}
\examples{
\donttest{
sites <- c("05403500", "05405000") # Two USGS streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,              envir=WisExample,
                                     sdate="1945-01-01", edate="2016-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
akq_summary(WisAKQ) # These line-wrapped values will change as record increases.
#     site beg_year end_year yr_range_str total_count count kendall_tau spearman_rho
# 05403500     1945     2016   1945--2016       18444  9242 -0.20468258   -0.3083903
# 05405000     1945     2016   1945--2016       26297 13724 -0.08380238   -0.1295017
#   median     L1L2  gfactor gfactor_emp
# 37.98102 125.1716 136.3617    139.3032
# 34.95030 155.0216 161.0996    176.1453 #}
}
\keyword{data extraction}
