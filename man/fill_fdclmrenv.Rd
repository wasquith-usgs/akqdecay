\name{fill_lmrfdcenv}
\alias{fill_lmrfdcenv}
\title{ Fill an R Environment with L-moments by Annual Flow-Duration Curve }
\description{
Perform tests for monotonic trend for each of 365 probabilities on the flow-duration curve with year for a succession of USGS streamgage identification numbers in an \R \code{environment} from \code{\link{fill_dvenv}}. The function \code{fill_lmrfdcenv} populates (\emph{fills}) with output from \code{\link{fdclmr}} as another \R \code{environment}.  In source code, this function is structurally almost identical to \code{\link{fill_tfdcenv}}.
}
\usage{
fill_lmrfdcenv(sites=NULL, dvenv=NULL, envir=NULL, silent=FALSE, ...)
}
\arguments{
  \item{sites}{An optional, likely partial, list of sites within \code{dvenv} for population into \code{envir}. This option means that a massive environment of daily values can be retained in the user's space but Asquith--Knight discharge decay analyses can be restricted to a smaller subset of sites. If \code{sites=NULL}, then internally all of the sites within \code{dvenv} will be processed, and this would be almost universally the way to use this function;}
  \item{dvenv}{An \R \code{environment} previously populated by \code{\link{fill_dvenv}};}
  \item{envir}{A user created (usually) \R \code{environment} by \code{new.env()};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{fdclmr}}.}
}
\value{
This function is used for its side effects on the \code{envir} argument but does return a count of the sites processed by \code{\link{fdclmr}}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{fdclmr}}, \code{\link{lmrfdc_table}}, \code{\link{fill_dvenv}}
}
\examples{
\dontrun{
wolfriver <- c("07030392", "07030500", "07031650",
               "07031660", "07031700", "07031740")
wolf.env <- new.env() # the standard declaration of an environment
fill_dvenv(wolfriver, envir=wolf.env) # wolf.env now filled with six tables.
wolf.fdclmr <- new.env() # the standard declaration of an environment
fill_lmrfdcenv(dvenv=wolf.env, envir=wolf.fdclmr)
# LMRs <- lmrfdc_table(wolf.fdclmr) # we could use the LMRs data.frame
# but in the L-moment ratio diagram below, we are incrementing the plot character.
lmomco::plotlmrdia(lmomco::lmrdia(), xlim=c(0,0.5), ylim=c(-0.1,0.3))
for(i in 1:length(wolfriver)) { # Now plot the L-moment ratios of these sites.
   tmp <- get(wolfriver[i], envir=wolf.fdclmr)
   points(tmp$T3, tmp$T4, pch=i, col=grey(0.2))
} # }
}
\keyword{environment filling}
\keyword{flow-duration curve}
\keyword{flow-duration curve (L-moments)}
