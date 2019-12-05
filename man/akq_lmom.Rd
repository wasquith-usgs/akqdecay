\encoding{utf8}
\name{akq_lmom}
\alias{akq_lmom}
\alias{akq_lmom_yeardecade}
\alias{akq_lmom_year}
\alias{akq_lmom_decade}
\title{L-moment Table from the Asquith--Knight Discharge Decay Analyses}
\description{
Extract the L-moments (Asquith, 2018) from the Asquith--Knight discharge decay analyses returned by \code{\link{akqdecay}} for a sequence of USGS streamgages contained within the \R \code{environment} that has been previously populated by \code{\link{fill_akqenv}}. The \code{\link{akq_lmom_yeardecade}} function is a parent function called by the functions \code{\link{akq_lmom_year}} and \code{\link{akq_lmom_decade}} in a self-evident manner. There are provisions for performing the Kendall Tau test for monotonic trend in time (Helsel and Hirsch, 2002) using the built-in \R \code{cor.test()} function.
}
\usage{
akq_lmom(           akqenv, silent=FALSE, ...)
akq_lmom_yeardecade(akqenv, which=c("year", "decade"),
                            silent=FALSE, tauenv=NULL, ...)
akq_lmom_year(      akqenv, ...) # wrapper on the above function
akq_lmom_decade(    akqenv, ...) # wrapper on the above function
}
\arguments{
  \item{akqenv}{The \R \code{environment} populated by \code{\link{fill_akqenv}}. This argument can also be an \R \code{list} directly from \code{\link{akqdecay}}; special accommodation is made;}
  \item{silent}{Suppress informative calls to \code{message()};}
  \item{which}{The argument controlling whether the returned L-moment summary table is on a by year or by decade basis;}
  \item{tauenv}{An optional \R \code{environment} in which the Taus and p-values for the Kendall Tau test for monotonic trend in time are held. If \code{NULL}, then logic for the trend testing is not triggered; however, if the \code{akqenv} is not an \code{environment} and directly the \R \code{list} results of \code{\link{akqdecay}}, then the Kendall Tau test is always triggered and available as attributes (\code{attr}) of the returned \R \code{data.frame}. The \code{tauenv} option is not used by \code{akq_lmom}. The \code{tauenv} will not work as expected (function with crash) if the processing is being made on L-moment tables based on \code{method="nochange"} passed to \code{\link{akqdecay}}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{data.frame} containing the L-moments for each streamgage depending on which form of retrieval is requested. The first of the \bold{Examples} shows extraction of the Kendall Tau test for a single streamgage in Oregon. The second example shows the L-moments for the period of record for two streamgages in Wisconsin. The third example shows extraction of the Kendall Tau test for the two Wisconsin streamgages. The fourth example shows manual extraction of sample sizes and first L-moment (arithmetic mean) and plots a pair of time series. For both styles of Kendall Tau test, the results are accessed by the \code{tau} and \code{tau_p.value} attribute or label in the \code{environment}. Thus, this function also is used for its side effects on the \code{tauenv} argument as applicable.

An attribute \code{skipped_sites} is attached to the returned data frame when \code{akenv} is an environment and contains, because most assuredly of sample size considerations, a vector of the sites that lacked an L-moment table (period-of-record, decadal, or yearly as applicable). Another attribute is \code{which} that contains the year or decade when either is triggered by the function call.
}
\author{ W.H. Asquith}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8, \url{http://www.amazon.com/dp/1463508417/}

Asquith, W.H., 2018, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.3.2 at \url{https://cran.r-project.org/package=lmomco}.

Asquith, W.H., 2014, Parameter estimation for the 4-parameter asymmetric exponential power distribution by the method of L-moments using R: Computational Statistics and Data Analysis, v. 71, pp. 955--970, \url{http://dx.doi.org/10.1016/j.csda.2012.12.013}

Helsel, D.R. and Hirsch, R.M., 2002, Statistical Methods in Water Resources: U.S. Geological Survey Techniques of Water Resources Investigations, book 4, chapter A3, 522 p., \url{https://pubs.usgs.gov/twri/twri4a3/}

Hosking, J.R.M., 1990, L-moments---Analysis and estimation of distributions using linear combinations of order statistics: Journal of the Royal Statistical Society, Series B, v. 52, pp. 105--124.
}
\seealso{\code{\link{akqdecay}}, \code{\link{fill_akqenv}}, \code{\link{akq_counts}}, \code{\link{akq_summary}}
}
\examples{
\dontrun{
# The Kendall Tau test is housed within the attributes of the data.frame when
# a single station is involved.
# USGS 11502500 Williamson River below Sprague River near Chiloquin, Oregon
AK <- akqdecay(dvget("11502500", sdate="1920-01-01", edate="2015-12-31"))
LY <- akq_lmom_year(AK)
taus <- attr(LY, "tau"); pvls <- attr(LY, "tau_p.value")
# Show Kendall's tau and p-value for the empirical gfactor vs year.
message("90th-percentile gfactor_emp: tau=",round(taus[5], digits=4),
                                  " [pval=",round(pvls[5], digits=4),"]")
# 90th-percentile gfactor_emp: tau=0.5691 [pval=0] #}

\dontrun{
sites <- c("05403500", "05405000") # Two streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,              envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
akq_lmom(WisAKQ)
#     site yr_range_str count   median     L1L2  gfactor gfactor_emp
# 05403500   1945--2014  8821 37.98102 120.2955 133.0099    136.0137
# 05405000   1945--2014 13293 34.52598 151.0085 157.0361    170.1584
#       L1       L2        T3        T4        T5        T6
# 63.06348 32.28971 0.5040707 0.3347492 0.2269736 0.1644391
# 72.84326 44.10004 0.5655227 0.3728133 0.2536264 0.1770801 #}

\dontrun{
# The Kendall tau test for both Wisconsin streamgages.
TY <- new.env() # to house the Kendall tau results by year
TD <- new.env() # to house the Kendall tau results by decade
LY <- akq_lmom_year(  WisAKQ, tauenv=TY); # print(TY$tau); print(TY$tau_p.value)
LD <- akq_lmom_decade(WisAKQ, tauenv=TD); # print(TD$tau); print(TD$tau_p.value)
message("90th-percentile gfactor_emp: tau=",round(TY$tau[5], digits=4),
                                  " [pval=",round(TY$tau_p.value[5], digits=4),"]")
# 90th-percentile gfactor_emp: tau=c(0.3051, 0.6704) [pval=c(0.0016, 0)] #}

\dontrun{
# See first example using the two Wisconsin streamgages.
TMP <- WisAKQ$"05405000"$lmoments$by_year
txt <- "1-day-over-day declines, in days per log10-cycle decline"
plot(TMP$year, TMP$count, xlab="Years", ylab=paste0("Number of ",txt))
mtext("USGS 05405000 BARABOO RIVER NEAR BARABOO, WI")

plot(TMP$year, TMP$L1,    xlab="Years", ylab=paste0("Mean ",     txt))
mtext("USGS 05405000 BARABOO RIVER NEAR BARABOO, WI") #}
}
\keyword{data extraction}
\keyword{L-moments}
\keyword{Kendall's Tau}
