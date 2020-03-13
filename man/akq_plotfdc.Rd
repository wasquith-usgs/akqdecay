\encoding{utf8}
\name{akq_plotfdc}
\alias{akq_plotfdc}
\title{Plot Decadal Flow-Duration Curves}
\description{
Plot decadal flow-duration curves.
}
\usage{
akq_plotfdc(gagefdc, site="", file=NA,
                     rev.decades=TRUE, showtitle=TRUE, ylim=NULL, ...)
}
\arguments{
  \item{gagefdc}{An \R \code{data.frame} from \code{\link{fdclmr}} with the \code{decade=TRUE} argument triggered;}
  \item{site}{A name or site identification number;}
  \item{file}{An optional file name for a portable document format file of the plot;}
  \item{showtitle}{A logical to trigger a helpful title for the plot that will include the \code{site}. This title is helpful in batch processing and report assembly because it acts as a caption of sorts;}
  \item{rev.decades}{The plot ordering of the decades. The default (\code{TRUE}) causes later decades with their larger symbol size to be plotted first and the earliest decades to be plotted last;}
  \item{ylim}{A means to insert vertical axis limits under user's control. This potentially could be useful when running lots of different streamgages or if the plotting of the horizontal axis labels are not completely plotted because of a weakness (as of March 2020) in the \pkg{lmomco} package function \code{add.lmomco.axis}; and}
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
gage  <- fdclmr(dvget(site, ignore.provisional=FALSE), decade=TRUE)
akq_plotfdc(gage, site=site) #}
}
\keyword{data output}
