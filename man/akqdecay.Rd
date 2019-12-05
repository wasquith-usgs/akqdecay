\encoding{utf8}
\name{akqdecay}
\alias{akqdecay}
\title{Asquith--Knight Discharge Decay Analyses}
\description{
Compute statistics associated with the today-to-tomorrow or \eqn{N}-day-over-day base-10 logarithm decline of daily streamflows. The preferred expression for this package is the inverse of the base-10 logarithm per day to form the days per unit log-cycle decline in streamflow.

The critical mathematical operation to understand for the \pkg{akqdecay} package is the differencing, and the \emph{entire \pkg{akqdecay} package is oriented around this single line} of code. Letting \code{logQ} be the base-10 logarithms of streamflow for which streamflows of zero are treated as \code{NA}, the differences are computed using the built-in \R \code{diff()} function:
\preformatted{
  delQ <- diff(logQ, lag=lag, differences=1)/lag
}
where \code{lag=1} implies a day (\dQuote{today}) to the following day (\dQuote{tomorrow}) difference, \code{differences=1} represents first-order differentiation, and the division by the \code{lag} standardizes the output to a log10-cycle change per day. The first example in \bold{Examples} demonstrates this (default) computation. The vector of \code{delQ} will become shortened in length relative to \code{logQ} by the count \code{lag}---thus, shortened by one value (one index of the vector) when the default \code{differences} and \code{lag} are used.

A setting of \code{differences} greater than 1 is applied recursively and is difficult to visualize, and gaps in record might not be properly handled. The \code{differences} is retained mostly for experimental reasons. A setting of \code{lag} greater than 1 means that skip overs are involved. The difference in number of days between indices of the flow vector are consulted in their relation to \code{lag} to handle unpadded gaps in record. This means that the \code{diff()} call on the logarithms of flow is mimicked by a \code{diff()} call on the dates, and inappropriate gaps in record not equal to the \code{lag} have the difference of the logarithms changed to \code{NA}. As a result, the total of the number of \code{NA}s encountered might not reflect number of zero-flow days (in context of settings of \code{differences} and \code{lag}). The handling of zero-flow days currently is an open research question for this package.

The \pkg{akqdecay} package inverts \code{delQ} prior to \emph{any statistical processing}. Let us refer to \code{1/delQ} as \eqn{\Psi(l,d)} where \eqn{l} is \code{lag} and \eqn{d} is \code{differences}. For brevity however, days per log-cycle may simply be denoted as \eqn{\Psi}. The inversion has the effect of changing the unit system to units of time per log-cycle change. The reasoning is that \eqn{\Psi} is in readily interpreted units of time. Asquith and Knight suggest that units of time provide a straightforward unit system to describe these extension computations to broad audiences (e.g. nontechnical stakeholders).

Further, the choice of days per unit log10-cycle change (decline) was chosen because \eqn{\Psi} values are in the same units as the persistent recession slope previously studied in Alabama (Bingham, 1982) and then again in Tennessee (Bingham, 1986). That slope was referred to as the \dQuote{\eqn{G_b}-factor} (the \dQuote{Bingham geologic factor}). Here in the \pkg{akqdecay} package, the \code{gfactor} (parametrical) or \code{gfactor_emp} (empirical) are used as monikers under which a specific definition of such a \dQuote{factor} is entwined with a probability level. The association of \eqn{\Psi}-like values to probability was not a subject of Bingham's studies.
}
\usage{
akqdecay(akdvtable, as.list=TRUE, as.ra7=FALSE, f=0.90,
                    method=c("decreasing", "increasing", "nochange"),
                    lag=1, differences=1, site="", type="gpa",
                    notable=FALSE, setgfna=FALSE, ...)
}
\arguments{
  \item{akdvtable}{A USGS daily-value of streamflow table with some extensions unique to this package as returned by \code{\link{dvget}};}
  \item{as.list}{Return an extensive list of computed tables of statistics;}
  \item{as.ra7}{Compute the \dQuote{RA7} statistic (like the \code{ra7()} function of the \pkg{EflowStats} package by the USGS that is not a dependency of the \pkg{akqdecay} package). For numerical comparison, it is critically important that \code{as.ra7} returns the median \emph{natural logarithm}, where as all other meaning of logarithms in the \pkg{akqdecay} package are base10 to be consistent with Bingham's earlier work. Also, this option, if set, trumps, the \code{lag} and \code{differences} values and assigns unity to each. This argument returns only the median of the decreasing or increasing days and the no change is by definition zero. As a final note, the \pkg{EflowStats} package substitutes 0.01 cubic feet per second for zero flow days---the \pkg{akqdecay} package tracks these as missing for purposes of computation, and thus in presence of zero flows, numerical differences between the two \dQuote{RA7} methods would result;}
  \item{f}{A nonexceedance probability (\eqn{F \in [0,1]}) on which to compute a quantile of the recession or rise estimate (see \bold{Note} labeled as \code{gfactor} and \code{gfactor_emp}), and the default is the 90th percentile;}
  \item{method}{Categories controlling the direction of \dQuote{decay} computation. The default is to look at statistics of the recession (\code{decreasing} to acquire \eqn{\check\Psi}). But rising (\code{increasing} to acquire \eqn{\hat\Psi}) limbs of the hydrograph or those periods without change (\code{nochange} to acquire \eqn{\ddot\Psi}) can be computed. To avoid user concern, users are alerted to not be concerned with the tables (many \code{NA} values) produced with \code{nochange} because logarithms are involved and day-over-day with no changes are thus degenerate---the various subtables (data frames) produced by the \code{akqdecay} function are thus mostly unpopulated for \code{nochange};}
  \item{lag}{The argument of the same name for the \code{diff()} function that is called internally;}
  \item{differences}{The argument of the same name for the \code{diff()} function that is called internally;}
  \item{site}{An optional streamgage identification number or other character string that will be used in replication in subordinate tables generated by \code{akqdecay}. It is deliberate that this function does not pickup the \code{site_no} from the daily-value table \code{akdvtable}. This permits the user to potentially intercept the site number or name and replace with their own for flexibility in categorical statistical analyses in their own custom scripts;}
  \item{type}{Distribution type of the \pkg{lmomco} package to estimate the Asquith--Knight\cr \eqn{G_f(F;\Theta)}-factor for the nonexceedance probability (\eqn{F}) passed by \code{f} and the distribution parameters in \eqn{\Theta}. If the \code{type=}\code{NA}, then the computation of the fitted distribution is short circuited and this is useful for speed during testing (for instance);}
  \item{notable}{If set, then the \code{table} element of the returned list when \code{as.list} is set is simply set to a table (data frame) with the \code{site} and \code{"not_requested"}. This is done mostly as a means to shorten the returned list and functionally this option has compatibility with \code{\link{akq_table}}. However, recomputation (\code{\link{gfredo}}) of empirical Gfactors would not be possible if \code{notable} is set. This occurs because access to the underlying sample of \eqn{\Psi} has been lost---the sample thrown away. Keep this in mind that perhaps \code{notable} is mostly an option for the developers;}
  \item{setgfna}{This option is the same as for \code{\link{gfredo}}, meaning if \code{setgfna} is set, then \code{type} is internally set to \code{NA}; and}
  \item{...}{Additional arguments to pass to control \code{\link{lmrdf2gfactor}}.}
}
\details{
The time units are important for further discernment because of the influence of \code{lag}. If the weekly change were computed using \code{lag=7}, the \code{delQ} would be interpretable as the \dQuote{log10-cycle change per day based on a weekly-based time difference.} Thus, \eqn{\Psi(7,1)} for \code{lag=7} would be the weeks for a log-cycle change.
\preformatted{
  logQ <- log10(c(130,122,121,100,95,93,92,91)) # 8 declining flow days
  median(1/abs(diff(logQ, lag=1)))     # days per log-cycle change [7 values]
  [1] 73.67673 # median days based on one  day of lag
  median(1/abs(diff(logQ, lag=7)) * 7) # days per log-cycle change [1 value ]
  [1] 45.18987 # median days based on one week of lag
}

\bold{What does \emph{days per log-cycle mean?}}  Some users are anticipated to be less comfortable with the idea of \emph{log-cycle} than others. A log-cycle, as used herein for base-10 logarithms, is a \emph{power of ten} or \emph{order of magnitude} change. For example and for a hypothetical streamgage, if the median \eqn{\Psi} or \eqn{\Psi_{\mathrm{med}}} computed from declining days over many decades was \eqn{73} days, then one might infer (statistically) that today's flow will drop by a power of ten in about \eqn{2.5} months. Suppose then that another nearby streamgage with similar climate and watershed size had \eqn{\Psi_{\mathrm{med}} = 34} days for a similar time period. The analyst might then conclude that different hydrologic processes exist between the two watershed to cause the first watershed to have about \eqn{1.5} months longer for streamflow to drop by a power of ten.

\bold{How might one interpret \eqn{\Psi}?} There are very many subtleties when interpreting statistics of \eqn{\Psi}, and hence the motivation for the \pkg{akqdecay} package to compute an immense array of statistics for later evaluation. But in general for \code{method=}\code{"decreasing"} and the other defaults, small values of \eqn{\Psi} represent the collapse of streamflow following rainfall-runoff (stormflow) events and reside in the left-tail of the distribution of \eqn{\Psi}, whereas large values of \eqn{\Psi} reside in the right-tail of the distribution and inherently are mostly associated with baseflow stability and hence potential groundwater and surface water interactions.
}
\value{
Various types of tables and statistics can be returned. If \code{as.ra7}, then the median of the declines or inclines (\code{decreasing=FALSE}) is returned---just a simple single value. If \code{as.list}\code{=FALSE}, then the time series of \code{decreasing} or \code{increasing} or \code{nochange} as days per log-cycle is returned. If \code{as.list}, then the following are returned:
  \item{table}{An \R \code{data.frame} containing columns for the site identifier, water year, calendar year, month, decade, date (forward bias, \dQuote{tomorrow's} streamflow), probability on the flow-duration curve (full period of record used), the respective daily flow for that day is returned (forward bias), and days per unit log-cycle change (decay) of \eqn{N}-day-over-day increments;}
  \item{counts}{A named \R \code{vector} containing the total number of daily values (same as \cr
\code{total_count} discussed with \code{summary} below), the number of \code{decreases}, the number of \code{increases}, the number of no changes (\code{nochanges}), and number of missing values (\code{NAs}). The meaning of \code{nochanges} is retricted to the \eqn{N}-day-over-day changes after logarithmic transformation whose differences are zero; this means that no changes in streamflow from zero flow to zero flow are not counted in this alogrithm;}
 \item{summary}{An \R \code{data.frame} with a summary of the period of record results absent the L-moments: \code{total_count} (inclusive of the unchanging increments in streamflow), \code{count}, \code{kendall_tau} and \code{spearman_rho}, \code{median}, \code{L1L2}, \code{gfactor}, and \code{gfactor_emp};}
  \item{lmoments}{An \R \code{list} of data frames of the L-moments for the period-of-record (\code{por}), L-moments \code{by_year} (without regard to whether a given calendar year had the streamgage active for the whole year), including the median and sample size (count of decreases [or increases]), and L-moments \code{by_decade} (without regard to completeness [is truly a full decade of record available]); and}
  \item{ifail}{A number code indicate the failure mode (0, no problems encountered; 1, no values for any processing; 2, no values after \code{NA} check on the \code{days_per_log} [the \eqn{\Psi} values].}
}
\note{
The authors (Asquith and Knight) suggest that particular quantiles of the distribution of \eqn{\Psi}  might be more germane than others for statistical regionalization studies. In particular, far into the right tail of the distribution of \eqn{\Psi} is of primary interest. Though the sample \code{median} is computed by \code{akqdecay}, a setting \code{f=0.5} would be the median of a fitted three-parameter (3p) generalized Pareto distribution (GPA), if \code{type="gpa"}, fit to the data, and this \dQuote{50th percentile} is thus emplaced as a \code{gfactor}. The choice of the GPA is the default; though this choice might not be optimal. The mathematics of the GPA are listed under \code{\link{lmrdf2gfactor}}. The choice for the GPA is based on experimental processing of more than 1,600 streamgages and review of the distribution of L-skew (\eqn{\tau_3}) and L-kurtosis (\eqn{\tau_4}) plotted on an L-moment ratio diagram (Asquith, 2011a,b).

A nonparametric estimate for the \code{f} is emplaced in \code{gfactor_emp}. The default of \code{f}\code{=0.90} is thus the 90th percentile. Optimal choice of \code{f}, should it materially exist as a constant or nearly so, is an \emph{open research problem}.

\bold{Correlation between \dQuote{Tomorrow's} Streamflow and \eqn{\Psi}:} Both Kendall Tau and Spearman Rho correlation coefficients (Helsel and Hirsch, 2002) are computed between \dQuote{tomorrow's} streamflow (units of cubic feet per second) and \eqn{\Psi} (units of days). These correlations are stored in the \code{summary} as explained above. The correlations are computed before the absolute value of \eqn{\Psi} is made---recall that these are always positive on return from this function with respect to the \code{method} choice of \code{decreasing} or \code{increasing}. A negative correlation coefficient, thus, means that \eqn{\Psi} decreases as streamflow increases.

\bold{Attributes:} The attributes of \dQuote{method,} \dQuote{lag,} \dQuote{differences,} \dQuote{probability,} and \dQuote{distribution_type} for the function arguments \code{method}, \code{lag}, \code{differences}, \code{probability}, and \code{distribution_type} are set by the built-in \R \code{attr()} function unless \code{as.ra7=TRUE}. This permits the user to probe the returned value for these settings if ever desired. If \code{as.list=FALSE}, then only the attributes of \dQuote{method,} \dQuote{lag,} and \dQuote{differences} are set.

\bold{Parametric Gfactor Computations:}  This package is intended for large data mining computations. To this end, a special trap for incomplete L-moments is made for protection against failure within the \pkg{lmomco} package. If the period-of-record L-moments do not test as valid by two checks. First, the L-moments must be valid through the logic of \code{lmomco::}\code{are.lmom.valid()}. Second, if either L-skew (\eqn{\tau_3}) or L-kurtosis (\eqn{\tau_4}) is \code{NA}, then the Gfactor is computed as \code{NA}. (This is mentioned because by design, it is easy to leak missing L-skew into \pkg{lmomco}'s distribution fitting and breakage occurs there and not within \pkg{akqdecay}. The function \code{\link{lmrdf2gfactor}} does this protection for yearly and decadal statistics.)
}
\author{ W.H. Asquith}
\references{
Asquith, W.H., 2011a, Distributional analysis with L-moment statistics using the R environment for statistical computing: Ph.D. dissertation, Texas Tech University, accessed on March 15, 2017 at \url{https://ttu-ir.tdl.org/ttu-ir/handle/2346/ETD-TTU-2011-05-1319}.

Asquith, W.H., 2011b, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace, [print-on-demand], ISBN 978--146350841--8, [reprinting of Asquith (2011a) with errata]

Asquith, W.H., 2018, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.3.2 at \url{https://cran.r-project.org/package=lmomco}.

Bingham, R.H., 1982, Low-flow characteristics of Alabama streams, USGS Water Supply Paper 2083, 27 p., \url{http://pubs.er.usgs.gov/publication/wsp2083}

Bingham, R.H., 1986, Regionalization of low-flow characteristics of Tennessee streams: U.S. Geological Survey Water-Resources Investigations Report 85--4191, 63 p., \url{http://pubs.usgs.gov/wri/wri85-4191/}

Helsel, D.R. and Hirsch, R.M., 2002, Statistical Methods in Water Resources: U.S. Geological Survey Techniques of Water Resources Investigations, book 4, chapter A3, 522 p., \url{https://pubs.usgs.gov/twri/twri4a3/}
}
\seealso{\code{\link{dvget}}, \code{\link{fill_akqenv}}
}
\examples{
\dontrun{
# USGS 07030392 Wolf River at Lagrange, Tennessee
DVs <- dvget("07030392", sdate="1995-10-01", edate="1995-12-31")
head(DVs) # We will look at the first decreasing day change.
#  agency_cd  site_no       Date Flow Flow_cd     site year month decade wyear
#4      USGS 07030392 1995-10-04   79       A 07030392 1995    10   1990  1996
#5      USGS 07030392 1995-10-05   78       A 07030392 1995    10   1990  1996
# Let us define "tomorrow" as Oct5 and "today" as Oct4. LAG = unity (1 day)
#   (log10(78) - log10(79))/LAG = -5.532E-3 --> abs(-5.532E-3) = 5.532E-3
#   and then invert to days per log-cycle change --> 1/5.532E-3 --> 180.75 days
AK <- akqdecay(DVs) # Perhaps Asquith--Knight discharge decay analyses
head(AK$table) # We can see the foundational computations:
#  site wyear year month decade       date        fdc fqc days_per_log
#        1996 1995    10   1990 1995-10-05 0.18279570  78    180.75049
# (The last column for pp_days_per_log is not shown for brevity.)
# Notice that a AK$table will shorten by one row because
#  log10(tomorrow) - log10(today) reduces the length by the LAG.
# Thus, we can only track one of the two flows entering into the computation
# Let us retain a "forward bias" and report "tomorrow," hence AK$table
# shows 78 cfs as the first row and retains it on the actual date 10/05/1995.}

\dontrun{
# Continuing the above example, let us look at then a statistic of
# the 52 declining days. Let us look first at the general computations
head(AK$lmoments$por) # The period of record statistics are here.
# site yr_range_str count  median     L1L2  gfactor gfactor_emp
#         1995--1995    52 51.4158 162.5754 194.3716    180.7505
#       L1       L2        T3      T4         T5         T6
# 83.71642 44.49143 0.3981854 0.17382 0.07210696 0.02502416
# We see the median declining days for one log10-cycle decline is about 51.4, and
# the sample size is 52. The 90th percentile (default) of the empirical
# distribution of the values is about 180.8 days. Finally, we can compare the
# median here to the "RA7" statistic.
RA7 <- akqdecay(DVs, as.ra7=TRUE)
print(RA7)
# natural_logarithm_median_decreasing
#                         0.04484609
names(RA7) <- NULL
1/log10(exp(RA7))
# [1] 51.34416 which approximately matches the 51.4 reported previously.}

\dontrun{
# USGS 08167000 Guadalupe River at Comfort, Texas
site <- "08167000"; my.data <- dvget("08167000")
recession_limb <- akqdecay(my.data, as.list=TRUE, method="decreasing", site=site)
   rising_limb <- akqdecay(my.data, as.list=TRUE, method="increasing", site=site)
   stable_limb <- akqdecay(my.data, as.list=TRUE, method="nochange",   site=site)
boxplot(days_per_log~year,   log="y", data=recession_limb$table)
boxplot(days_per_log~decade, log="y", data=   rising_limb$table,
        ylim=c(1,2000)); mtext("RISING LIMB")
boxplot(days_per_log~decade, log="y", data=recession_limb$table,
        ylim=c(1,2000)); mtext("RECESSION LIMB")
plot(count~year,   log="y", data=stable_limb$lmoments$by_year  )
plot(count~decade, log="y", data=stable_limb$lmoments$by_decade) #}
}
\keyword{Gfactor}
\keyword{Asquith--Knight discharge decay analyses}
\keyword{Kendall's Tau}
\keyword{Spearman's Rho}
