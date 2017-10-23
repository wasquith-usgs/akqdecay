\name{fill_akqenv_parallel}
\alias{fill_akqenv_parallel}
\title{ Fill an R Environment by Parallel Processing with Asquith--Knight Discharge Decay Analyses }
\description{
Compute Asquith--Knight discharge decay analyses for a succession of USGS streamgage identification numbers in an \R \code{environment} from \code{\link{fill_dvenv}} and populates with output from \code{\link{akqdecay}} as another \R \code{environment}. This function is intended to be used as an alternative to \code{\link{fill_akqenv}} on unix-like platforms only. The \pkg{mcparallelDo} package is used, though this is not a required package for \pkg{akqdecay}. This function, thus, will only operate properly if \pkg{mcparallelDo} is in fact installed. The process is setup to parallel no more than six sites at a time, wait until all six jobs are finished, and then launch another ensemble. This might not be ideal because if a long-record site is among four very short-record sites, then there are fractions of time without all potential jobs running. However, the design of \code{fill_akqenv_parallel} has proven to be effective, robust, and yields much acceleration benchmarked against \code{\link{fill_akqenv}}.
}
\usage{
fill_akqenv_parallel(sites=NULL, dvenv=NULL, envir=NULL, silent=FALSE, ...)
}
\arguments{
  \item{sites}{The vector of sites within \code{dvenv} for processing. This option means that a massive environment of daily values can be retained in the user's space but Asquith--Knight discharge decay analyses can be restricted to a smaller subset of sites. If \code{sites=NULL}, then internally all of the sites within \code{dvenv} will be processed, and this would be almost universally the way to use this function;}
  \item{dvenv}{An \R \code{environment} previously populated by \code{\link{fill_dvenv}};}
  \item{envir}{A user created (usually) \R \code{environment} by \code{new.env()};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{akqdecay}}.}
}
\value{
  This function is used for its side effects on the \code{envir} argument but does return a count of the sites processed by \code{\link{akqdecay}}.
}
\note{
The \pkg{mcparallelDo} package (\emph{version 1.1.0}) is used for parallel processing. However, the implementation within the \code{fill_akqdenv_parallel} function, whether by lack of fully understanding of \pkg{mcparallelDo} features or not, results in zombies left on the process table for each process fork. These are harmless per se when the number of sites processed is relatively small but they can consume a lot to \emph{all physical memory} resources when processing repeatedly during testing or during real-world massive processing (thousands of sites each with say many tens of hundred thousands of daily-mean streamflows). The zombies remain until the parent \R session is closed, and to migigate, a tracking (accounting) of the PID is seems to be required.

Exploration of code and communication with the author, indicates that \pkg{mcparallelDo} does not preserve the PID of forked \R sessions through is use of a \code{jobName}---that is, \pkg{mcparallelDo} does not consult the PID issued from its own calls to the \pkg{parallel} package. The following changes to \code{mcparallelDo/}\code{R/}\code{mcparallelDo.R} within the \code{mcparallelDo()} function is made to store the PID under the \code{jobName}. The changes are shown in the syntax of the unix-like \code{diff} (compare files line by line) system function:
\preformatted{
  187,189c187,189
  <   jobName <- R.utils::tempvar(".mcparallelDoJob",
  <                               value = parallel::mcparallel({try(code)}),
  <                               envir = targetEnvironment)
  ---
  >   p <- parallel::mcparallel({try(code)})
  >   jobName <- R.utils::tempvar(paste0(".mcparallelDoJob","-",p$pid,"-"),
  >                               value = p, envir = targetEnvironment)
}
The modification shown alters lines 187--189 to change the prefix of a temporary variable name by appending a hypen, the PID, and another hyphen to the \dQuote{\code{.mcparallelDoJob}} root. The function \code{mcparallelDo::}\code{mcparallelDo()} is run in its verbose mode in order to return the temporary \code{jobName}. A \code{killZombies()} function is defined inside \code{fill_akqdenv_parallel}. After all of the six spawned processes complete, that function uses the stored job names, which is technically after the point that \pkg{mcparallelDo} has already deleted these temporary variables. The \code{killZombies()} extracts the PIDs from the job name, and six \R \code{system()} calls are made with unix-like \code{kill PID} to kill each of the zombies.
}
\author{ W.H. Asquith}
\seealso{\code{\link{fill_akqenv}}
}
\examples{
\dontrun{
# See fill_dvenv() Examples for the creation of wolf.env used here.
akqwolf.env1 <- new.env() # the standard declaration of an environment
akqwolf.env2 <- new.env() # the standard declaration of an environment
system.time(fill_akqenv(         dvenv=wolf.env, envir=akqwolf.env1))
system.time(fill_akqenv_parallel(dvenv=wolf.env, envir=akqwolf.env2)) #}
}
\keyword{Asquith--Knight discharge decay analyses}
\keyword{environment filling}
\keyword{parallel processing}
