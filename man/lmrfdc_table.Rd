\name{lmrfdc_table}
\alias{lmrfdc_table}
\title{ Convert an R Environment of L-moments by Annual Flow-Duration Curve to a Dataframe }
\description{
Extract the raw computations from output of \code{\link{fdclmr}} and place the results into an \R \code{data.frame}. This function will insert a row of \code{NA}'s into the returned table of the changes from one streamgage to the next using the built-in \R \code{rbind()} function. The inserted line will look something like row 6873:
\preformatted{
            site year   n nzero           L1           L2   ... etc ...
6871   023358685 2015 365     0 1.051835e+00 2.113052e-01   ... etc ...
6872   023358685 2016 274     0           NA           NA   ... etc ...
6873        <NA>   NA  NA    NA           NA           NA   ... etc ...
6874    02335870 1984  92     0           NA           NA   ... etc ...
6875    02335870 1985 365     0 1.451423e+00 1.618747e-01   ... etc ...
   ... etc ...
}
By providing this \code{NA}, as shown, graphical operations on the table will be able to \dQuote{lift the pen} when the successive streamgage is encountered.
}
\usage{
lmrfdc_table(lmrfdcenv, NAline.insert=TRUE, silent=FALSE, ...)
}
\arguments{
  \item{lmrfdcenv}{A user created by \code{\link{fill_lmrfdcenv}};}
  \item{NAline.insert}{A logical that can be used to suppress the \code{NA} line insertion described in the \bold{Description} section;}
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
\note{
The logic here is almost exactly that of \code{\link{akq_table}} with only differences in the last couple of lines of internal code. This is not a great design thus, but because the different themes of day-over-day changes and then statistics of the flow-duration curve, it is felt it is prudent to have separate implementations.
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
