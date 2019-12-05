\encoding{utf8}
\name{akq_write}
\alias{akq_write}
\title{Write Tables from Data Frames}
\description{
Write the \R \code{data.frame} produced by either \code{\link{akq_counts}},  \code{\link{akq_lmom}}, \code{\link{akq_summary}}, and  \code{\link{akq_table}} to the file system via a wrapper on the built-in \R \code{write.table()} function. The output has quotation marks stripped and no row names [\code{row.names()}].
}
\usage{
akq_write(df, file=NA, sep=";", digits=6, silent=FALSE,
              header="# optional header string", ...)
}
\arguments{
  \item{df}{An \R \code{data.frame};}
  \item{file}{The file name, and if \code{NA}, then this function has no effect;}
  \item{sep}{The separator for the output file;}
  \item{digits}{The number of digits to round for the numerical columns of the \code{data.frame};}
  \item{silent}{Suppress informative calls to \code{message()};}
  \item{header}{An optional header to be written above the output via the built-in \R \code{sink()}. The user need not worry about a terminal line ending as this function appends a newline before the sinking; and}
  \item{...}{Additional arguments to pass to control the \code{write.table()} function.}
}
\value{
  This function has no returned value and is intended for its side effects.
}
\note{
The built-in \R \code{write.table()} function is used. This function is called by hardwired options \code{append=}\code{TRUE}, \code{quote=}\code{FALSE}, and \code{row.names=}\code{FALSE}, but the \code{...} argument is passed to function \code{write.table()}, thus a user can access all of the other options of that function. For example, suppose a user does not desire the column names, then \code{col.names=}\code{FALSE} could be included in the \code{...} arguments.
}
\author{ W.H. Asquith}
\seealso{\code{\link{akqdecay}}, \code{\link{akq_scibase}}
}
\examples{
\dontrun{
sites <- c("05403500", "05405000") # Two streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,      envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
WisLM <- akq_lmom(WisAKQ)
akq_write(WisLM, file="BarabooRiverLM.txt",
                 header="# L-moments of the 1-day declines") #}
}
\keyword{data output}
