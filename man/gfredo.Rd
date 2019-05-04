\name{gfredo}
\alias{gfredo}
\title{ Recompute Gfactor from the L-moment Data Frames from Asquith--Knight Discharge Decay Analyses }
\description{
Recompute parametric Gfactors (\code{gfactor}; \eqn{G^\mathrm{type}_f}) from L-moment \code{lmoments$} data frames for \code{por}, \code{by_year}, and \code{by_decade}. Also recompute empirical Gfactors (\code{gfactor_emp}; \eqn{G^\mathrm{emp}_f}) from the \code{table} data frame. Specifically, the function modifies the \code{gfactor} and \code{gfactor_emp} columns in the output structure of \code{\link{akqdecay}}. The Gfactor specifically is \eqn{G_f(F;\Theta_\mathrm{type})} for nonexceedance probability \eqn{F \in [0,1]} and parameters \eqn{\Theta} for a given type of distribution computed by the method of L-moments (Asquith, 2018). This function is a wrapper (abstraction layer) on \code{\link{lmrdf2gfactor}} and serves as a means for updating the \R \code{environment} from \code{\link{fill_akqenv}}. The \code{gfredo} function can thus be used to avoid recalling \code{\link{fill_akqenv}} as a means to update the \eqn{G_f} estimation but avoids a huge amount of redundant computation---there is no need to recompute the L-moments, for example.
}
\usage{
gfredo(akqenv, f=0.90, type="gpa", which=c("por", "year", "decade"),
               all=TRUE, showdiffsum=FALSE, setgfna=FALSE, silent=FALSE, ...)
}
\arguments{
  \item{akqenv}{The \R \code{environment} populated by \code{\link{fill_akqenv}}. This argument can also be a direct \R \code{list} from \code{\link{akqdecay}}, special accommodation is made;}
  \item{f}{Nonexceedance probability (\eqn{F \in [0,1]}), and the default is the 90th percentile;}
  \item{type}{Distribution type of the \pkg{lmomco} package to estimate the \eqn{G_f} for the probability \code{f}.  If the type is \code{NA}, then the computation of the fitted distribution is short circuited. The default is the three-parameter generalized Pareto (GPA), and mathematics of the GPA are listed under \code{\link{lmrdf2gfactor}};}
  \item{which}{The style or type of L-moment storage. This argument is named similarly to the \code{which} argument of the \code{\link{akq_lmom}};}
  \item{all}{A logical and if set, each of the \code{"por"}, \code{"year"}, and \code{"decade"} are swept through regardless of the setting of \code{which} in order to update the entire L-moment structure as built by \code{\link{akqdecay}}. It is almost asured that users want to keep this as \code{all=}\code{TRUE}, and the option is mostly present for the developer;}
  \item{showdiffsum}{This function computes new \eqn{G_f} and as a result, it might be useful to review the effect of the change in \eqn{G_f} from old to new. This argument triggers a simple \code{print} around the  built-in \R \code{summary()} function;}
  \item{setgfna}{If set, then Gfactor (\code{gfactor}; \eqn{G_f}) is set to \code{NA}. This can be a useful function if the user wants to focus on empirical Gfactor (\code{gfactor_emp}) or desires rapid updating;}
  \item{silent}{Suppress informative calls to \R \code{message}; and}
  \item{...}{Additional arguments to pass (see source code to ascertain flexible usage).}
}
\value{
  This function is intended to be used for its side effects on the \code{akenv}. Though if that argument is an \R \code{data.frame} then that after \eqn{G_f} modification is returned.
}
\references{
Asquith, W.H., 2018, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.3.2 at \url{https://cran.r-project.org/package=lmomco}
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmrdf2gfactor}}
}
\examples{
\dontrun{
# USGS 11502500 Williamson River below Sprague River near Chiloquin, Oregon
AK <- akqdecay(dvget("11502500", sdate="1920-01-01", edate="2015-12-31"))
AK <- gfredo(AK, f=0.80, type="gpa", showdiffsum=TRUE)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  25.65   48.81   74.29  108.30  177.40  276.20
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  43.44   64.18   96.26  119.10  182.40  219.00
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  132.2   132.2   132.2   132.2   132.2   132.2
#  The 132.2 is the day change from old to new for period of record. # }

\dontrun{
sites <- c("05403500", "05405000") # Two streamgages in Wisconsin
WisExample <- new.env(); fill_dvenv( sites,      envir=WisExample,
                                     sdate="1945-01-01", edate="2015-12-31")
WisAKQ <- new.env(); fill_akqenv(dvenv=WisExample, envir=WisAKQ, type="gev")
#      Note that the generalized extreme value distribution is used (gev).
gfredo(WisAKQ, f=0.95, all=FALSE, showdiffsum=TRUE) # 95th percentile
#  Recomputing gfactors for 05403500 (1/2) for por -- got'em
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  -61.48  -61.48  -61.48  -61.48  -61.48  -61.48
#  Recomputing gfactors for 05405000 (2/2) -- got'em
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  -87.71  -87.71  -87.71  -87.71  -87.71  -87.71
gfredo(WisAKQ, f=0.95, all=FALSE, which="year")   # now redo the years
gfredo(WisAKQ, f=0.95, all=FALSE, which="decade") # now redo the decades

# Better yet, let the function by default redo all three to the 89th percentile.
# but convert back to the generalized Pareto distribution.
gfredo(WisAKQ, f=0.89, all=TRUE, type="gpa", silent=TRUE) #}
}
\keyword{Gfactor}
\keyword{data editing}
