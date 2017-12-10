\name{fdclmr}
\alias{fdclmr}
\title{ Compute the First Six L-moments of the Flow-Duration Curve for Each Year or Decade (and Other Statistics) }
\description{
Compute the first six sample L-moments (\eqn{\hat\lambda_1}, \eqn{\hat\lambda_2}, \eqn{\hat\tau_3}, \eqn{\hat\tau_4}, \eqn{\hat\tau_5}, and \eqn{\hat\tau_6}; Asquith [2011a,b, 2017]) of the flow-duration curve (FDC) for each year passing a maximum number of missing days permissible. The missing days are implicitly assumed to be randomly distributed within a given year. (Most partial years in USGS databases are those on years of activation or discontinuance of a streamgage.)

The function also computes the minimum, median, and maximum in conjunction with the 7-day annual minimum flow (7-days consecutive) and the 7-day annual maximum flows. The total number of flow values and the number of zero flows (see \bold{Note}) also are tracked. Finally, the function can compute on a decadal basis and when it does the quantiles of the FDC are computed too (see extended discussion in the \bold{Value} section).
}
\usage{
fdclmr(akdvtable, missing.days=7, site="", decade=FALSE,
                  log=FALSE, subzero=NULL, plusit=1, verbose=FALSE, ...)
}
\arguments{
  \item{akdvtable}{A USGS daily-value of streamflow table with some extensions unique to this package as returned by \code{\link{dvget}};}
  \item{missing.days}{The number of permissible missing days in a given year on which to still compute the annual flow-duration curve and use that year in the trend test; The default is one week and it is advised to not exceed that count. The streamflow values are first reduced by removing \code{NA} before comparing the total number of values for a year against the value 365 minus \code{missing.days}. This argument is scaled up by 10 if \code{decade=TRUE} (computations are internal, user just needs to think about how many missing days per year might be acceptable, but internally these could be a consecutive block, for example, if a week was permitted for the decadal computations 70 missing days in a row would be permissible);}
  \item{site}{An optional streamgage identification number (or other character string). It is deliberate that this function does not pickup the \code{site_no} from the daily-value table \code{akdvtable}. This permits the user to potentially intercept the site number or name and replace with their own for flexibility in categorical statistical analyses in their own custom scripts;}
  \item{decade}{A logical to switch to decadal computations. A decade is already available in the \code{akdvtable} and see its documentation in \code{\link{dvget}} for the definition of decade for this package.}
  \item{log}{Should base-10 logarithms of the daily streamflow be computed before L-moments computed;}
  \item{subzero}{A value for substitution for zero values when \code{log=TRUE}. This feature is subject to change because of the nonoptimality of such substitution;}
  \item{plusit}{A value for added to \emph{also} values (not just those that are zero);}
  \item{verbose}{A logical controlling a potentially helpful message that prints the site and the current year as they are looped through. This is similar to the \code{silent} in \cr \code{\link{fill_lmrfdcenv}}, but named different here for discrete functionality between \code{fdclmr} and \code{\link{fill_lmrfdcenv}}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
An \R \code{data.frame} of mutable length but containing the following columns for \code{decade=FALSE}:
  \item{site}{An echo of the \code{site};}
  \item{n}{The number of days (values) entering into the L-moment computations;}
  \item{nzero}{The number of zero flow days that would not enter into L-moment computations if and only if \code{log=TRUE};}
  \item{min}{The minimum daily mean streamflow;}
  \item{median}{The median daily mean streamflow;}
  \item{max}{The maximum daily mean streamflow;}
  \item{min7day}{The minimum 7-day mean streamflow;}
  \item{max7day}{The maximum 7-day mean streamflow;}
  \item{L1}{The arithmetic mean (\eqn{\lambda_1});}
  \item{L2}{L-scale (\eqn{\lambda_2});}
  \item{T3}{L-skew (\eqn{\tau_3}), third L-moment ratio;}
  \item{T4}{L-kurtosis (\eqn{\tau_4}), fourth L-moment ratio;}
  \item{T5}{Fifth unnamed L-moment ratio (\eqn{\tau_5}), but the term L-cinco occassionally could be used; and}
  \item{T6}{Fifth unnamed L-moment ratio (\eqn{\tau_6}), but the term L-seis could be used.}
  The output is the effectively the same with \code{decade=TRUE}, except \code{year} becomes \code{decade}, the \code{median}, \code{min7day}, and \code{max7day} are dropped. Some 27 columns holding quantiles of the empirical FDC are then included. These have column titles such as \code{f50} (median), \code{f0.1} (0.1 percentile), or \code{f99.98} (99.98 percentile). These column titles are percentiles and not nonexceedance probabilities so division by 100 would be needed if the titles were parsed (leading \code{f} stripped) to dynamically compute the probabilities. These percentiles are fixed internally. Finally because \eqn{1 - 1/(10 \times 365.25+1) = 0.99972}, the computed percentile extremum will match the \code{min} and \code{max} where 365.25 is the approximate average number of days and 10 represents a decade. The \code{quantile()} function with its argument \code{type=6} (Weibull) is used internally to compute the empirical FDC.
}
\note{
Concerning zero flows, these are used if \code{log=FALSE}. Otherwise additional adjustments are made for protection against \code{-Inf}. The germane logic for the arguments \code{subzero} and \code{plusit} is
\preformatted{
   if(! is.null(subzero)) fdc[fdc == 0] <- subzero
   if(! is.null(plusit )) fdc <- fdc + plusit
}
So with this logic arrangement it is possible, though the reasoning is uncertain for both operations to be used simultaneously (a warning is issued), to substitute the zero values and add something to all the values.
}
\author{ W.H. Asquith}
\references{
Asquith, W.H., 2011a, Distributional analysis with L-moment statistics using the R environment for statistical computing: Ph.D. dissertation, Texas Tech University, accessed on March 15, 2017 at \url{https://ttu-ir.tdl.org/ttu-ir/handle/2346/ETD-TTU-2011-05-1319}.

Asquith, W.H., 2011b, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace, [print-on-demand], ISBN 978--146350841--8, [reprinting of Asquith (2011a) with errata]

Asquith, W.H., 2017, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.2.9 at \url{https://cran.r-project.org/package=lmomco}.
}
\seealso{\code{\link{fill_lmrfdcenv}}, \code{\link{lmrfdc_table}}
}
\examples{
# See Examples under fill_lmrfdcenv and elsewhere in this documentation.
}
\keyword{flow-duration curve}
\keyword{flow-duration curve (L-moments)}

