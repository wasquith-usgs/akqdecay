\name{fill_dvpartenv}
\alias{fill_dvpartenv}
\title{ Fill an R Environment with Streamflow Partitioning using Asquith--Knight Daily-Value Table }
\description{
Compute streamflow partitioning for a succession of USGS streamgage identification numbers in an \R \code{environment} from \code{\link{fill_dvenv}} and populates (\emph{fills}) with output from \code{\link{dvpart}} as another \R \code{environment}.
}
\usage{
fill_dvpartenv(sites=NULL, dvenv=NULL, envir=NULL, silent=FALSE, ...)
}
\arguments{
  \item{sites}{An optional, likely partial, list of sites within \code{dvenv} for population into \code{envir}. This option means that a massive environment of daily values can be retained in the user's space but streamflow partitioning analyses can be restricted to a smaller subset of sites. If \code{sites=NULL}, then internally all of the sites within \code{dvenv} will be processed, and this would be almost universally the way to use this function;}
  \item{dvenv}{An \R \code{environment} previously populated by \code{\link{fill_dvenv}};}
  \item{envir}{A user created (usually) \R \code{environment} by \code{new.env()};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{akqdecay}}.}
}
\value{
  This function is used for its side effects on the \code{envir} argument but does return a count of the sites processed by \code{\link{dvpart}}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{dvpart}}, \code{\link{fill_dvenv}}
}
\examples{
\dontrun{
# See fill_dvenv() Examples for the creation of wolf.env used here.
partwolf.env <- new.env() # the standard declaration of an environment
fill_dvpartenv(dvenv=wolf.env, envir=partwolf.env) #}
}
\keyword{streamflow partitioning}
\keyword{baseflow}
\keyword{environment filling}
