\name{dvpartenv2mnpartenv}
\alias{dvpartenv2mnpartenv}
\title{ Convert an R Environment with Daily Streamflow Partitioning to an R Environment with Monthly Aggregations }
\description{
Converted processing from \code{\link{fill_dvpartenv}} to monthly aggregated streamflow stored in another \R \code{environment}.
}
\usage{
dvpartenv2mnpartenv(sites=NULL, dvpartenv=NULL, envir=NULL, silent=FALSE,...)
}
\arguments{
  \item{sites}{An optional, likely partial, list of sites within \code{dvenv} for population into \code{envir}. This option means that a massive environment of daily values can be retained in the user's space but streamflow partitioning analyses can be restricted to a smaller subset of sites. If \code{sites=NULL}, then internally all of the sites within \code{dvenv} will be processed, and this would be almost universally the way to use this function;}
  \item{dvpartenv}{An \R \code{environment} previously populated by \code{\link{fill_dvpartenv}};}
  \item{envir}{A user created (usually) \R \code{environment} by \code{new.env()};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{akqdecay}}.}
}
\value{
This function is used for its side effects on the \code{envir} argument but does return a count of the sites processed by \code{\link{dvpart2mnpart}}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{dvpart}}, \code{\link{dvpart2mnpart}}
}
\examples{
\dontrun{
# See fill_dvenv() Examples for the creation of wolf.env used here.
# Six USGS streamgages of the Wolf River in Tennessee
wolfriver <- c("07030392", "07030500", "07031650",
               "07031660", "07031700", "07031740")
wolf.env <- new.env() # the standard declaration of an environment
fill_dvenv(wolfriver, envir=wolf.env) # wolf.env now filled with six tables.

cdas <- sites_to_SpatialPointsDataFrame(wolfriver)$CDA

pdvwolf.env <- new.env() # the standard declaration of an environment
# Now, let us trigger the gap filler and the messages disappear.
fill_dvpartenv(dvenv=wolf.env, envir=pdvwolf.env, cdas=cdas, fillgaps=TRUE)

pmnwolf.env <- new.env()
dvpartenv2mnpartenv(dvpartenv=pdvwolf.env, envir=pmnwolf.env) # }
}
\keyword{streamflow partitioning}
\keyword{baseflow}
\keyword{environment filling}
\keyword{monthly streamflow}

