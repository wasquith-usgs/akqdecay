\encoding{utf8}
\name{gsid2str}
\alias{gsid2str}
\title{Convert Numbers to USGS Station Identification Numbers}
\description{
Convert numbers to USGS station identification numbers. The USGS has many surface water monitoring stations with a leading 0 (zero) in the official number. Reasonably so, \R when viewing such numbers as, well, numbers (e.g. \code{as.numeric}) removes the leading zero. (For example, the second number in the \bold{Examples}, though written with leading zero, does not retain this zero when \R interprets the code.) It is a common problem having to return the zero in a string or character (e.g. \code{as.character}) representation. If a number is 7, 9, or 11 digits long, prepend a leading zero to form a string comprised of 8, 10, or 12 digits. \bold{There is a major fault in this approach---some sites can have 9  or 11 digit numbers, which is a fact not known to Asquith until March 2017 after almost 25 years with USGS.}
}
\usage{
gsid2str(siteNumbers, hack=FALSE, ...)
}
\arguments{
  \item{siteNumbers}{A vector of USGS station identification numbers as numeric or character (a cast to \code{as.character} is made internally);}
  \item{hack}{If set, then the function works as described, otherwise the \code{siteNumbers} are returned \emph{unmodified}; and}
  \item{...}{Additional arguments to pass; though not expected to ever be used.}
}
\value{
  This function returns \code{siteNumbers} recast as either 8-, 10-, or 12-digit station numbers in the form of character strings.
}
\author{ W.H. Asquith}
\seealso{\code{\link{dvget}}
}
\examples{
print(gsid2str(c(8167000, 08167500, 816750001, 81675000001), hack=TRUE))
# [1] "08167000"     "08167500"     "0816750001"   "081675000001"

\dontrun{
# USGS 08080700 Running Water Draw at Plainview, Texas
# Literally "a view of a plain" and Asquith has passed through the town innumerable
# times since a child and has serviced the streamgage many times.
RunningWaterDraw <- dvget(gsid2str(8080700, hack=TRUE),
                          sdate="2009-01-01", edate="2016-12-31") #}
}
\keyword{data retrieval}
\keyword{data editing}
