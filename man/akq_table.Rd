\name{akq_table}
\alias{akq_table}
\title{ Tabulate (Extract) Raw Discharge Decay Computations }
\description{
Extract the raw computations from output of \code{\link{akqdecay}} and place the results into an \R \code{data.frame}. This function will insert a row of \code{NA}'s into the returned table of the changes from one streamgage to the next using the built-in \R \code{rbind()} function. The inserted line will look something like row 9069:
\preformatted{
9068  05403500  2016 2016     3   2010 2016-03-26 9.211251e-01 1040 ... etc ...
9069      <NA>    NA   NA    NA     NA       <NA>           NA   NA ... etc ...
9070  05405000  1945 1945     1   1940 1945-01-04 1.045350e-02   90 ... etc ...
   ... etc ...
}
By providing this \code{NA}, as shown, graphical operations on the table will be able to \dQuote{lift the pen} when the successive streamgage is encountered.
}
\usage{
akq_table(akqenv, NAline.insert=TRUE, type=c("akqdecay", "other"),
                                      silent=FALSE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} previously populated by \code{\link{fill_akqenv}}. This argument can also be an \R \code{list} directly from \code{\link{akqdecay}}; special accommodation is made;}
  \item{NAline.insert}{A logical that can be used to suppress the \code{NA} line insertion described in the \bold{Description} section;}
  \item{type}{What type of table is sought? The \code{"akqdecay"} means that the \code{Z$table} of the list from \code{\link{fill_akqenv}} is to be extracted from the value \code{Z} in the environment stored under the site. The \code{"other"} just means that the environment is storing data frames, and this type is for environments from \code{\link{fill_dvenv}} or \code{\link{fill_dvpartenv}}. Internally, the source code has some different column accessing based on \code{type} that is partly an artifact of early design decision;}
  \item{silent}{Suppress informative calls to \code{message()}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  An \R \code{data.frame} containing the counts for each streamgage.
  \item{site}{The streamgage identification number;}
  \item{wyear}{The water year;}
  \item{year}{The calendar year;}
  \item{month}{The month;}
  \item{decade}{The decade of the calendar year;}
  \item{date}{The date of the \dQuote{tomorrow} streamflow (forward bias);}
  \item{fdc}{The nonexceedance probability from the flow-duration curve for the period of record;}
  \item{fqc}{The \dQuote{tomorrow} streamflow for the date;}
  \item{days_per_log}{Days per log cycle change of streamflow and dependent on the setting for \code{method} in \code{\link{akqdecay}}; and}
  \item{pp_days_per_log}{Weibull plotting position of \code{days_per_log}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{akqdecay}}
}
\examples{
\dontrun{
sites <- c("05403500", "05405000") # Two USGS streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,      envir=WisExample,
                                     sdate="1945-01-01", edate="2014-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ)
TAB <- akq_table(WisAKQ); TAB$color <- as.numeric(as.factor(TAB$site))+1
plot(qnorm(TAB$pp_days_per_log), TAB$days_per_log, type="p", col=TAB$color,
     cex=0.6, log="y", xlab="Standard Normal Variate", ylab="days per log-cycle")

plot(TAB$date, TAB$days_per_log, log="y", type="p",
     col=TAB$color, xlim=as.Date(c("1960-01-01", "1960-06-01"))) #}
}
\keyword{data extraction}
