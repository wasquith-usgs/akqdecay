\encoding{utf8}
\name{fill_akqenv}
\alias{fill_akqenv}
\title{Fill an R Environment with Asquith--Knight Discharge Decay Analyses}
\description{
Compute Asquith--Knight discharge decay analyses for a succession of USGS streamgage identification numbers in an \R \code{environment} from \code{\link{fill_dvenv}} and populates (\emph{fills}) with output from \code{\link{akqdecay}} as another \R \code{environment}. This function is intended to be used just ahead of extraction functions such as \code{\link{akq_counts}}, \code{\link{akq_lmom}}, and \code{\link{akq_summary}}.
}
\usage{
fill_akqenv(sites=NULL, dvenv=NULL, envir=NULL, silent=FALSE, ...)
}
\arguments{
  \item{sites}{An optional, likely partial, list of sites within \code{dvenv} for population into \code{envir}. This option means that a massive environment of daily values can be retained in the user's space but Asquith--Knight discharge decay analyses can be restricted to a smaller subset of sites. If \code{sites=NULL}, then internally all of the sites within \code{dvenv} will be processed, and this would be almost universally the way to use this function;}
  \item{dvenv}{An \R \code{environment} previously populated by \code{\link{fill_dvenv}};}
  \item{envir}{A user created (usually) \R \code{environment} by \code{new.env()};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{akqdecay}}.}
}
\value{
  This function is used for its side effects on the \code{envir} argument but does return a count of the sites processed by \code{\link{akqdecay}}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{akqdecay}}, \code{\link{fill_dvenv}}, \code{\link{akq_summary}}, \code{\link{fill_akqenv_parallel}}
}
\examples{
\dontrun{
# See fill_dvenv() Examples for the creation of wolf.env used here.
akqwolf.env <- new.env() # the standard declaration of an environment
fill_akqenv(dvenv=wolf.env, envir=akqwolf.env) #}
}
\keyword{Asquith--Knight discharge decay analyses}
\keyword{environment filling}
