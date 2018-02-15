\name{dvoverlap}
\alias{dvoverlap}
\title{ Find Sites with Overlapping Complete Years of Daily Values }
\description{
Find sites with overlapping complete years of daily values from the daily-value environment.
}
\usage{
dvoverlap(dvenv, begyr=1987, endyr=2016, type=c("wyear", "year"),
                 missing.days=7, silent=TRUE, ...)
}
\arguments{
  \item{dvenv}{An \R \code{environment} previously populated by \code{\link{fill_dvenv}};}
  \item{begyr}{Starting year;}
  \item{endyr}{Ending year;}
  \item{type}{Are calendar or water years desired;}
  \item{missing.days}{The number of permissible missing days in a given year on which to still report it as a complete year;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass, if ever used.}
}
\value{
  An \R \code{vector} of the site numbers from the \code{dvenv} meeting the criteria as set forth by the arguments to the function.
}
\author{ W.H. Asquith}
\seealso{\code{\link{fill_dvenv}}
}
\examples{
\dontrun{
# Six USGS streamgages of the Wolf River in Tennessee
wolfriver <- c("07030392", "07030500", "07031650",
               "07031660", "07031700", "07031740")
wolf.env <- new.env() # the standard declaration of an environment
fill_dvenv(wolfriver, envir=wolf.env) # wolf.env now filled with six tables.
sites_overlap <- dvoverlap(wolf.env, begyr=1987, endyr=2016, silent=FALSE)
# No sites overlap, but change the years and we see
sites_overlap <- dvoverlap(wolf.env, begyr=2000, endyr=2010, silent=FALSE)
# dvoverlap() for 07030392 has 11 'full' years: (2000,2010)
# dvoverlap() for 07030500 --- does not meet criteria
# dvoverlap() for 07031650 has 11 'full' years: (2000,2010)
# dvoverlap() for 07031660 --- does not meet criteria
# dvoverlap() for 07031700 --- does not meet criteria
# dvoverlap() for 07031740 has 11 'full' years: (2000,2010)

# Now let us double check by visualization
for(site in ls(wolf.env)) {
    tmp <- get(site, wolf.env)
    plot(tmp$Date, tmp$Flow, type="l", log="y")
    mtext(site)
} # }
}
\keyword{data summary}
