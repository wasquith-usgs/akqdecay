\name{lmrfdc2df}
\alias{lmrfdc2df}
\title{ Convert an R Environment of L-moments by Annual Flow-Duration Curve to a Dataframe }
\description{
Convert \code{\link{fill_tfdcenv}}.
}
\usage{
lmrfdc2df(envir, silent=FALSE, ...)
}
\arguments{
  \item{envir}{A user created by \code{\link{fill_tfdcenv}};}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
An \R \code{data.frame} of mutable length but containing the following columns:
  \item{site}{An echo of the \code{site};}
  \item{n}{The number of days (values) entering into the L-moment computations;}
  \item{nzero}{The number of zero flow days that would not enter into L-moment computations if and only if \code{log=TRUE};}
  \item{L1}{The arithmetic mean (\eqn{\lambda_1});}
  \item{L2}{L-scale (\eqn{\lambda_2});}
  \item{T3}{L-skew (\eqn{\tau_3}), third L-moment ratio;}
  \item{T4}{L-kurtosis (\eqn{\tau_4}), fourth L-moment ratio;}
  \item{T5}{Fifth unnamed L-moment ratio (\eqn{\tau_5}), but the term L-cinco occassionally could be used; and}
  \item{T6}{Fifth unnamed L-moment ratio (\eqn{\tau_6}), but the term L-seis could be used.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{fdclmr}}, \code{\link{fill_tfdcenv}}
}
\examples{
# See Examples under fill_lmrfdcenv and elsewhere in this documentation.
}
\keyword{environment filling}
\keyword{flow-duration curve}
\keyword{flow-duration curve (L-moments)}
