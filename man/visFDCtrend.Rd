\name{visFDCtrend}
\alias{visFDCtrend}
\title{ Visualization of Kendall's Tau Trend Test throughout the Annual Daily-Flow-Duration Curves  }
\description{
Draw plots of the Kendall's tau trend test of the annual daily-flow-duration curves housed in the \code{environment} from \code{\link{fill_tfdcenv}}.
}
\usage{
visFDCtrend(fdcenv=NULL, file=NA, alpha=0.05, fast=FALSE, ...)
}
\arguments{
  \item{fdcenv}{An \R \code{environment} previously populated by \code{\link{fill_tfdcenv}}. This argument can also be a \code{data.frame} if a single site is desired (see \bold{Examples});}
  \item{file}{An optional file name for a portable document format graphics file;}
  \item{alpha}{The statistical significance (two-tailed), it will be divided in two when the large sample approximation for Kendall's tau is needed. Otherwise is it is used as is for comparison to the p-value from the \code{cor.test()} function  built-in to \R that had already been called by \code{\link{fdctrend}} via \code{\link{fill_tfdcenv}};}
  \item{fast}{A logical controlling whether the progressively lengthing returned \code{data.frame} is actually made. This can be really CPU heavy and potentially unneeded by the user. If set, just \code{"fast option used"} will be retured, and all graphics proceed normally; and}
  \item{...}{Additional arguments to pass to the \code{par()} function built-in to \R that is called internally.}
}
\value{
An \R \code{data.frame} of all the annual daily-flow-duration curve tables from the \code{tfdcenv} with columns:
\item{site}{The site identification;}
\item{count}{The sample size as determined by the number of year columns stemming from \code{\link{fdctrend}};}
\item{prob}{The nonexceedance probability into the annual daily-flow-duration curve;}
\item{estimate}{The estimate of Kendall's tau from the \code{cor.test} function; and}
\item{p.value}{The p-value from the \code{cor.test()} function for \code{method="kendall"}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{fill_tfdcenv}}
}
\examples{
# See Examples under fill_tfdcenv() and elsewhere in this documentation.
\dontrun{
DV <- new.env()
sites <- c("08093100","08093500")
fill_dvenv(sites, envir=DV, edate="2016-09-30")
TFDC <- new.env()
n <- fill_tfdcenv(dvenv=DV, envir=TFDC) # number fdctrend() processed
AllFDCtrend <- visFDCtrend(TFDC$"08093500") # one plot is shown}
}
\keyword{flow-duration curve}
\keyword{flow-duration curve (trend)}
\keyword{Kendall tau}
