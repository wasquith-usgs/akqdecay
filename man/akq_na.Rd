\name{akq_na}
\alias{akq_na}
\title{ Check within an Environment for NA (Missing) Daily Asquith--Knight Discharge Decay Analyses or Zero Rows of Daily Values }
\description{
This function has two effects depending on the anticipated type, which means the \emph{contents}, for an \R \code{environment}. First, check by streamgage for exclusively one \code{NA} stored in the \R \code{environment} created by \code{\link{fill_akqenv}} and return those site numbers. Second, check by streamgage for zero rows of the daily-value \code{data.frame} created by \code{\link{fill_akqenv}} and return those site numbers.
}
\usage{
akq_na(akqenv, silent=FALSE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} previously populated by \code{\link{fill_akqenv}};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{vector} containing the sites for which the entry \code{akqenv} is \code{NA}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{fill_akqenv}}
}
\examples{
\dontrun{
sites <- c("05403500", "05405000") # Two USGS streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,              envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
WisAKQ$"99999999" <- NA; # insert a fake site with simple NA
print(akqna(WisAKQ)) #}
}
\keyword{data editing}
