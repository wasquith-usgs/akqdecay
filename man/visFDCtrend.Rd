\encoding{utf8}
\name{visFDCtrend}
\alias{visFDCtrend}
\title{Visualization of Kendall's Tau Trend Test throughout the Annual Daily-Flow-Duration Curves}
\description{
Draw plots of the Kendall's tau trend test of the annual daily-flow-duration curves housed in the \code{environment} from \code{\link{fill_tfdcenv}}. Calendar year is the basis as provided by the \code{\link{fdctrend}}, which is the real algorithm deployment. Some have referred to the visualization as the \dQuote{Quantile-Kendall plot.}
}
\usage{
visFDCtrend(fdcenv=NULL, file=NA, alpha=0.05, fast=FALSE, site=NA,
                         notitle=FALSE, showflowregime=TRUE,
                         width=7, height=5, ...)
}
\arguments{
  \item{fdcenv}{An \R \code{environment} previously populated by \code{\link{fill_tfdcenv}} that in turn dispatches to \code{\link{fdctrend}}. This argument can also be a \code{data.frame} if a single site is desired (see \bold{Examples});}
  \item{file}{An optional file name for a portable document format graphics file;}
  \item{alpha}{The statistical significance (two-tailed), it will be divided in two when the large sample approximation for Kendall's tau is needed. Otherwise is it is used as is for comparison to the p-value from the \code{cor.test()} function  built-in to \R that had already been called by \code{\link{fdctrend}} via \code{\link{fill_tfdcenv}};}
  \item{fast}{A logical controlling whether the progressively lengthening \code{data.frame} of the return is created. This \code{data.frame} can be really CPU heavy and potentially unneeded by many users. If set, just \code{"fast option used"} will be returned. With either setting, all graphics proceed the same;}
  \item{site}{A possible optional string for the site number or other information to annotate on the graphic and used as the site identifier in the returned \code{data.frame} otherwise just plain-old \dQuote{site} is used. Finally, this \code{site} is only picked up if the \code{fdcenv} was a \code{data.frame} and the \code{site} is not missing;}
  \item{notitle}{A logical to trigger the title of the plot that is constructed internally from the \code{site} and other information such as year interval and number of years of record represented in the plot;}
  \item{showflowregime}{A logical to trigger annotation about where the low-flow, median, and high-flow regimes reside on the plot;}
  \item{width}{The width of the portable document format graphics device that matches the \code{width} of the \code{pdf()} function;}
  \item{height}{The height of the portable document format graphics device that matches the \code{height} of the \code{pdf()} function; and}
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
\seealso{\code{\link{fill_tfdcenv}}, \code{\link{fdctrend}}
}
\examples{
# See Examples under fill_tfdcenv() and elsewhere in this documentation.
\dontrun{
DV <- new.env()
sites <- c("08093100","08093500")
fill_dvenv(sites, envir=DV, edate="2016-09-30")
TFDC <- new.env()
n <- fill_tfdcenv(dvenv=DV, envir=TFDC) # number fdctrend() processed
AllFDCtrend <- visFDCtrend2(TFDC$"08093500") # one plot is shown
AllFDCtrend <- visFDCtrend2(TFDC$"08093500", site="") # no labeling
AllFDCtrend <- visFDCtrend2(TFDC) # both plots are shown }
}
\keyword{flow-duration curve}
\keyword{flow-duration curve (trend)}
\keyword{Kendall tau}
\keyword{Quantile-Kendall plot}
\concept{quantile kendall}
\concept{quantile Kendall}
