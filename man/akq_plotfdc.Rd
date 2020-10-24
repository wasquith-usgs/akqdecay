\encoding{utf8}
\name{akq_plotfdc}
\alias{akq_plotfdc}
\title{Plot Decadal Flow-Duration Curves}
\description{
Plot decadal flow-duration curves on a streamgage-by-streamgage basis using the data frame assembled by the \code{fdclmr(..., decade=TRUE)} function.
}
\usage{
akq_plotfdc(gagefdc, site="", file=NA, rev.decades=TRUE,
                     showtitle=TRUE, ylim=NULL, return.fdc=FALSE, ...)
}
\arguments{
  \item{gagefdc}{An \R \code{data.frame} from \code{\link{fdclmr}} with the \code{decade=TRUE} argument triggered;}
  \item{site}{A name or site identification number;}
  \item{file}{An optional file name for a portable document format file of the plot;}
  \item{showtitle}{A logical to trigger a helpful title for the plot that will include the \code{site}. This title is helpful in batch processing and report assembly because it acts as a caption of sorts;}
  \item{rev.decades}{The plot ordering of the decades. The default (\code{TRUE}) causes later decades with their larger symbol size to be plotted first and the earliest decades to be plotted last;}
  \item{ylim}{A means to insert vertical axis limits under user's control. This potentially could be useful in some circumstances;}
  \item{return.fdc}{A logical to trigger returning of the flow-duration curve data frame used to construct the plot. This data frame sees removal of decades for which some streamflows were present but the decade was not complete and all the quantiles were simply \code{NA} (missing); and}
  \item{...}{Additional arguments to pass.}
}
\value{
  This function has no returned value and is intended for its side effects.
}
\author{ W.H. Asquith}
\seealso{\code{\link{fdclmr}}
}
\examples{
\donttest{
site <- "08167000"
gage  <- fdclmr(dvget(site, ignore.provisional=FALSE), decade=TRUE)
akq_plotfdc(gage, site=site) #}
}
\keyword{data output}
