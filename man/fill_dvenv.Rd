\name{fill_dvenv}
\alias{fill_dvenv}
\title{ Fill an R Environment with Streamflow Partitioning Analyses by Streamgage }
\description{
Fill an \R \code{environment} with \R \code{data.frames} of daily-mean streamflow for USGS streamgages. This function is a wrapper (abstraction layer) on the \code{\link{dvget}} function.
}
\usage{
fill_dvenv(siteNumbers, envir=NULL,  sdates=NULL,  edates=NULL,
                                    flowlos=NULL, flowhis=NULL,
                        silent=FALSE, ...)
}
\arguments{
  \item{siteNumbers}{A vector of USGS streamgage identification numbers;}
  \item{envir}{The \R \code{environment} to use and required to be user created by \code{new.env()};}
  \item{sdates}{A vector of starting dates (possibly unique by streamgage) to feed the similarly named argument of \code{\link{dvget}} with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed. Use \code{""} for earliest date, and for convenience, a vector of length 1 (unity) is recycled to form a vector with as many elements as length if \code{siteNumbers};}
  \item{edates}{A vector of ending dates (possibly unique by streamgage) to feed the similarly named argument of \code{\link{dvget}} with string format of \dQuote{YYYY-MM-DD} for year (YYYY), month (MM), and day (DD) respectively padded by zeros as needed. Use \code{""} for latest date, and for convenience, a vector of length 1 (unity) is recycled to form a vector with as many elements as length if \code{siteNumbers};}
  \item{flowlos}{A vector of lower streamflow thresholds (likely unique by streamgage) to feed the similar named argument of \code{\link{dvget}};}
  \item{flowhis}{A vector of upper streamflow thresholds (likely unique by streamgage) to feed the similar named argument of \code{\link{dvget}};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{dvget}}.}
}
\value{
  An \R \code{environment} is returned.
}
\note{
Emergency intervention or other types of bulk filtering on the environment can be accomplished with code like this for an environment named \code{old_dv}:
\preformatted{
  for(site in ls(old_dv)) {
     ZZ <- get(site, envir=old_dv)
     ZZ <- ZZ[ZZ$year > 1984 & ZZ$year < 1997,] # just keep 1985--1996
     assign(site, ZZ, envir=old_dv)
  }
}
However, such an operation would be destructive to the \code{old_dv} environment. An independent copy of the environment could first be made:
\preformatted{
  new_dv <- list2env(as.list(newdv)) # Note that "new_dv <- old_dv" is not a copy!
  for(site in ls(new_dv)) {
     ZZ <- get(site, envir=new_dv)
     ZZ <- ZZ[ZZ$month <= 3 | ZZ$month >= 10,] # just keep Oct--Dec, Jan--Mar
     assign(site, ZZ, envir=new_dv)
  }
}
}
\author{ W.H. Asquith}
\seealso{\code{\link{dvget}}, \code{\link{akqdecay}}, \code{\link{fill_akqenv}}, \code{\link{fill_dvpartenv}}, \code{\link{dvoverlap}}
}
\examples{
\dontrun{
# Six USGS streamgages of the Wolf River in Tennessee
wolfriver <- c("07030392", "07030500", "07031650",
               "07031660", "07031700", "07031740")
wolf.env <- new.env() # the standard declaration of an environment
fill_dvenv(wolfriver, envir=wolf.env) # wolf.env now filled with six tables.
# and wolf.env is shown within the Examples of fill_akqenv and fill_dvpartenv. #}
}
\keyword{data retrieval}
\keyword{environment filling}
