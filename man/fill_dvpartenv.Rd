\encoding{utf8}
\name{fill_dvpartenv}
\alias{fill_dvpartenv}
\title{Fill an R Environment with Streamflow Partitioning using Asquith--Knight Daily-Value Table}
\description{
Compute streamflow partitioning for a succession of USGS streamgage identification numbers in an \R \code{environment} from \code{\link{fill_dvenv}} and populates (\emph{fills}) with output from \code{\link{dvpart}} as another \R \code{environment}.
}
\usage{
fill_dvpartenv(sites=NULL, dvenv=NULL, envir=NULL, cdas=NULL,
                           fillgaps=FALSE, silent=FALSE, ...)
}
\arguments{
  \item{sites}{An optional, likely partial, list of sites within \code{dvenv} for population into \code{envir}. This option means that a massive environment of daily values can be retained in the user's space but streamflow partitioning analyses can be restricted to a smaller subset of sites. If \code{sites=NULL}, then internally all of the sites within \code{dvenv} will be processed, and this would be almost universally the way to use this function;}
  \item{dvenv}{An \R \code{environment} previously populated by \code{\link{fill_dvenv}};}
  \item{envir}{A user created (usually) \R \code{environment} by \code{new.env()};}
  \item{cdas}{Contributing drainage areas of the \code{sites}. The vector lengths are expected to be the same. If \code{cdas=NULL}, then \code{NA}s are set with length equal to that of \code{sites}. These will become the \code{cda} argument of \code{\link{dvpart}};}
  \item{fillgaps}{A logical to trigger the record gap infiller within \code{\link{dvpart}} to log-linearly prorate raw streamflow (\code{Flow}) across record gaps, then perform streamflow separation, and then pack the separated streamflows back onto the input that still retains gaps in record. This gap filler does not actually then appear to create synthetic flow record---this is a very important point;}
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
# Six USGS streamgages of the Wolf River in Tennessee
wolfriver <- c("07030392", "07030500", "07031650",
               "07031660", "07031700", "07031740")
wolf.env <- new.env() # the standard declaration of an environment
fill_dvenv(wolfriver, envir=wolf.env) # wolf.env now filled with six tables.

cdas <- sites_to_SpatialPointsDataFrame(wolfriver)$CDA

partwolf.env <- new.env() # the standard declaration of an environment
fill_dvpartenv(dvenv=wolf.env, envir=partwolf.env, cdas=cdas, fillgaps=FALSE)
# It is advised to pay attention to the messages about the gaps in the records
# from the above call. Only 07030392 appears to have "complete" record

# Now, let us trigger the gap filler and the messages disappear.
fill_dvpartenv(dvenv=wolf.env, envir=partwolf.env, cdas=cdas, fillgaps=TRUE)

# Now just to finish out a demonstration, serialize the data frames
wolfdf <- akq_table(partwolf.env, type="other") # and then write to
akq_write(wolfdf, file="wolf_baseflows.txt") # the file system. }
}
\keyword{streamflow partitioning}
\keyword{baseflow}
\keyword{environment filling}
\keyword{streamflow separation}
