\encoding{utf8}
\name{akq_plotfdc}
\alias{akq_plotfdc}
\title{Plot Decadal Flow-Duration Curves}
\description{
Plot decadal flow-duration curves.
}
\usage{
akq_plotfdc(gagefdc, site="", file=NA, showtitle=TRUE, ...)
}
\arguments{
  \item{gagefdc}{An \R \code{data.frame} from \code{\link{fdclmr}} with the \code{decade=TRUE} argument triggered;}
  \item{site}{A name or site identification number;}
  \item{file}{An optional file name for a portable document format file of the plot;}
  \item{showtitle}{A logical to trigger a helpful title for the plot that will include the \code{site}. This title is helpful in batch processing and report assembly because it acts as a caption of sorts; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  This function has no returned value and is intended for its side effects.
}
\author{ W.H. Asquith}
\seealso{\code{\link{fdclmr}}
}
\examples{
\dontrun{
site <- "08167000"
dv   <- dvget(site, ignore.provisional=FALSE)
akq  <- akqdecay(dv)
gage  <- fdclmr(dv, decade=TRUE)
akq_plotfdc(gage) #}
}
\keyword{data output}
