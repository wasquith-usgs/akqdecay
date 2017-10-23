\name{akq_scibase}
\alias{akq_scibase}
\title{ Asquith--Knight Discharge Decay Analyses Converted to Plain-Text Files Suitable for USGS ScienceBase-like Release}
\description{
Extract all of the various results of Asquith--Knight discharge decay analyses and write these into parallel named plain-text output files. This function is a wrapper on calls to \code{\link{akq_write}} for the tables extracted by \code{\link{akq_counts}}, \code{\link{akq_lmom}}, \code{\link{akq_lmom_year}}, \code{\link{akq_lmom_decade}}, \code{\link{akq_summary}}, and \code{\link{akq_table}} (optional) utility functions. ScienceBase is a contemporaneous (\emph{c.}2017) distribution mechansim for certain USGS datasets.
}
\usage{
akq_scibase(akqenv, basename="SciBase_", extension=".txt",
                    dotable=FALSE, doyearlmr=FALSE, dodecadelmr=FALSE,
                    silent=FALSE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} previously populated by \code{\link{fill_akqenv}}. This can also be an \R \code{list} directly from \code{\link{akqdecay}}; special accommodation is made;}
  \item{basename}{The leading component of the output file names;}
  \item{extension}{The extension to append to the output file names;}
  \item{dotable}{Should \code{\link{akq_table}} also be called---this can be CPU and file system intensive;}
  \item{doyearlmr}{Should the call to \code{\link{akq_lmom}} dumping the yearly L-moments and Gfactors be made---this can be CPU and file system intensive;}
  \item{dodecadelmr}{Should the call to \code{\link{akq_lmom}} dumping the decadal L-moments and Gfactors be made---this will be 10 times less CPU and file system intensive than if \code{doyearlmr} is set. Try turning this one (\code{dodecadelmr}) on first;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass to control \code{\link{akq_write}}.}
}
\value{
  This function has no returned value and is intended for its side effects.
}
\author{ W.H. Asquith}
\seealso{\code{\link{akq_write}}
}
\examples{
\dontrun{
akq_scibase(akqdecay(dvget("08167000"), site="08167000"), dotable=TRUE) #}

\dontrun{
sites <- c("05403500", "05405000") # Two streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,              envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
akq_scibase(WisAKQ) #}
}
\keyword{data extraction}
\keyword{data output}
\keyword{text file output}
\keyword{USGS ScienceBase}
