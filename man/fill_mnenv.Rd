\encoding{utf8}
\name{fill_mnenv}
\alias{fill_mnenv}
\title{Fill an R Environment with Monthly Streamflow by Streamgage}
\description{
Fill an \R \code{environment} with \R \code{data.frames} of monthly streamflow for USGS streamgages. This function is a wrapper (abstraction layer) on the \code{\link{mnget}} function. Users interested in monthly values might be especially interested in the monthly computations based on the daily streamflows but with the inclusion of separated streamflow in \code{\link{dvpart}} and \code{\link{fill_dvpartenv}} (see \bold{Examples} in \code{\link{dvpart}} for a demonstration).
}
\usage{
fill_mnenv(siteNumbers, envir=NULL,  sdates=NULL,  edates=NULL,
                                    flowlos=NULL, flowhis=NULL,
                        silent=FALSE, ...)
}
\arguments{
  \item{siteNumbers}{A vector of USGS streamgage identification numbers;}
  \item{envir}{The \R \code{environment} to use and required to be user created by \code{new.env()};}
  \item{sdates}{A vector of starting dates (possibly unique by streamgage) to feed the similarly named argument of \code{\link{mnget}} with string format of \dQuote{YYYY-MM} for year (YYYY) and month (MM) respectively padded by zeros as needed. Use \code{""} for earliest date, and for convenience, a vector of length 1 (unity) is recycled to form a vector with as many elements as length if \code{siteNumbers};}
  \item{edates}{A vector of ending dates (possibly unique by streamgage) to feed the similarly named argument of \code{\link{mnget}} with string format of \dQuote{YYYY-MM} for year (YYYY) and month (MM) respectively padded by zeros as needed. Use \code{""} for latest date, and for convenience, a vector of length 1 (unity) is recycled to form a vector with as many elements as length if \code{siteNumbers};}
  \item{flowlos}{A vector of lower streamflow thresholds (likely unique by streamgage) to feed the similar named argument of \code{\link{mnget}};}
  \item{flowhis}{A vector of upper streamflow thresholds (likely unique by streamgage) to feed the similar named argument of \code{\link{mnget}};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{mnget}}.}
}
\value{
  An \R \code{environment} is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{mnget}}
}
\examples{
\dontrun{
# Six USGS streamgages of the Wolf River in Tennessee
wolfriver <- c("07030392", "07030500", "07031650",
               "07031660", "07031700", "07031740")
wolf.env <- new.env() # the standard declaration of an environment
fill_mnenv(wolfriver, envir=wolf.env) # wolf.env now filled with six tables.#}
}
\keyword{data retrieval}
\keyword{environment filling}
\keyword{monthly streamflow}
