\encoding{utf8}
\name{akq_rm}
\alias{akq_rm}
\title{Remove or Extract Site Numbers from Copied Package Environments}
\description{
Remove from either the environments related to calls to \code{\link{fill_akqenv}} or \code{\link{fill_dvenv}} a vector of site numbers and return a copy as an \R \code{list}. This is a simple function saving marginally on number of lines of code for scripting by the user, but with the \code{invert} logical, flexibility of using \pkg{akqdecay} package is compressed into a simple operation.
}
\usage{
akq_rm(siteNumbers, envir, invert=FALSE, ...)
}
\arguments{
  \item{siteNumbers}{A vector of USGS streamgage identification numbers. Any \code{NA} entries are removed and then a unique list of the site numbers is used. After the \code{NA} are removed and if then the length of the vector is zero, then \code{as.list(envir)} is returned with an informative warning message;}
  \item{envir}{The \R \code{environment} previously populated by \code{\link{fill_akqenv}} or  \code{\link{fill_dvenv}};}
  \item{invert}{Instead of deleting and returning the remainder, with \code{invert=}\code{TRUE}, return an \code{environment} of sites identified by \code{siteNumbers}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{list} is returned. This list needs to be converted back to an environment using \code{list2env} after and not within this function because of memory inflation as \R works to contain extra environmental information.
}
\author{ W.H. Asquith}
\seealso{\code{\link{fill_akqenv}}, \code{\link{fill_dvenv}}
}
\examples{
\dontrun{
sites <- c("05403500", "05405000") # Two USGS streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,              envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
nWisAKQ <- list2env(akq_rm("05405000", WisAKQ))
              akq_counts(nWisAKQ) # three columns removed below
#      site total_count decreases increases nochanges NAs
#  05403500       17714      8821      6011      2873   9 #}
}
\keyword{data editing}
