\name{fdclmr}
\alias{fdclmr}
\title{ Compute the First Six L-moments of the Flow-Duration Curve for Each Year }
\description{
The
}
\usage{
fdclmr(akdvtable, missing.days=7, site="",
                  log=TRUE, subzero=NULL, plusit=1, verbose=FALSE, ...)
}
\arguments{
  \item{akdvtable}{A USGS daily-value of streamflow table with some extensions unique to this package as returned by \code{\link{dvget}};}
  \item{missing.days}{The number of permissible missing days in a given year on which to still compute the annual daily-flow-duration curve and use that year in the trend test; The default is one week and it is advised to not exceed that count. The streamflow values are first reduced by removing \code{NA} before comparing the total number of values for a year against the value 365 minus \code{missing.days};}
  \item{site}{An optional streamgage identification number (or other character string). It is deliberate that this function does not pickup the \code{site_no} from the daily-value table \code{akdvtable}. This permits the user to potentially intercept the site number or name and replace with their own for flexibility in categorical statistical analyses in their own custom scripts;}
  \item{log}{Should base-10 logarithms of the daily streamflow be computed before L-moments computed;}
  \item{subzero}{A value for substitution for zero values when \code{log=TRUE}. This feature is subject to change because of the nonoptimality of such substitution;}
  \item{plusit}{A value for added to \emph{also} values (not just those that are zero);}
  \item{verbose}{A logical controlling a potentially helpful message that prints the site and the current year as they are looped through. This is similar to the \code{silent} in \cr \code{\link{fill_lmrfdcenv}}, but named different here for discrete functionality between \code{fdclmr} and \code{\link{fill_lmrfdcenv}}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
An \R \code{data.frame} of mutable length but containing the following columns:
  \item{site}{An echo of the \code{site};}
  \item{n}{The number of days (values) entering into the L-moment computations;}
  \item{nzero}{The number of zero flow days that would not enter into L-moment computations if and only if \code{log=TRUE};}
  \item{L1}{The arithmetic mean (\eqn{\lambda_1});}
  \item{L2}{L-scale (\eqn{\lambda_2});}
  \item{T3}{L-skew (\eqn{\tau_3}), third L-moment ratio;}
  \item{T4}{L-kurtosis (\eqn{\tau_4}), fourth L-moment ratio;}
  \item{T5}{Fifth unnamed L-moment ratio (\eqn{\tau_5}), but the term L-cinco occassionally could be used; and}
  \item{T6}{Fifth unnamed L-moment ratio (\eqn{\tau_6}), but the term L-seis could be used.}
}
\note{
The germane logic for the arguments \code{subzero} and \code{plusit} is
\preformatted{
   if(! is.null(subzero)) fdc[fdc == 0] <- subzero
   if(! is.null(plusit))  fdc <- fdc + plusit
}
So with this logic arrangement it is possible, though reasoning uncertain, to substitute the zero values and add something to all the values.
}
\author{ W.H. Asquith}
\references{
Asquith, W.H., 2011a, Distributional analysis with L-moment statistics using the R environment for statistical computing: Ph.D. dissertation, Texas Tech University, accessed on March 15, 2017 at \url{https://ttu-ir.tdl.org/ttu-ir/handle/2346/ETD-TTU-2011-05-1319}.

Asquith, W.H., 2011b, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace, [print-on-demand], ISBN 978--146350841--8, [reprinting of Asquith, 2011a, with errata]

Asquith, W.H., 2017, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.2.7 at \url{https://cran.r-project.org/package=lmomco}.
}
\seealso{\code{\link{fill_lmrfdcenv}}, \code{\link{lmrfdc_table}}
}
\examples{
# See Examples under fill_lmrfdcenv and elsewhere in this documentation.
}
\keyword{flow-duration curve}
\keyword{flow-duration curve (L-moments)}

