\name{akqdecay-package}
\docType{package}
\alias{akqdecay-package}
\title{Asquith--Knight Discharge Decay Analyses}
\description{
The \pkg{akqdecay} package is a narrowly focused \R package with a purpose to compute distributional statistics of the \eqn{N}-day-over-day declines (recession limb, \emph{decreasing} day-over-day) of base-10 logarithms of daily-mean streamflow for U.S. Geological Survey (USGS) streamflow-gaging stations (streamgages). Computational alterations, however, can be made to compute \eqn{N}-day-over-day inclines (rising limb, \emph{increasing} day-over-day). Prodigious output tables are computed.

Statistical properties of streamflow recession provide evidence of hydrologic processes such as surface water and groundwater interactions. For example, a persistent streamflow recession slope, previously studied in Alabama (Bingham, 1982) and then again in Tennessee (Bingham, 1986), was referred to as the \dQuote{\eqn{G_b}-factor} (the \dQuote{Bingham geologic factor}). Bingham sought an approach for understanding and mapping general connectivity between surface water and groundwater in those study areas. The \eqn{G_b}-factor has been useful for other studies involving statistical regionalization of streamflow.

The \eqn{G_b}-factor was derived through graphical methods on hand-selected hydrographs for various periods of time. Bingham drew hydrographs on semi-log paper (log-streamflow) and estimated a generalized decay slope (ECO---confirm this statement). The inverse of this slope has units of days per unit log10-cycle change in streamflow and such units are attractive for interpretation as well as simple convenience. The units state the number of days until an order of magnitude or power of 10 change.

The conceptual underpinning of the \pkg{akqdecay} package is the study of the persistance of the recession limb of streamflow hydrographs. This is accomplished through computation of an extensive array of distributional statistics of very many one-day-over-day slopes (\eqn{\Psi}) computed for a streamgage. The authors (Asquith and Knight) speculate that massive-scale data processing of days having declining streamflow for a vast number of streamgages will embody hydrologic concepts similar those associated with the \eqn{G_b}-factor without resorting to selection of ideal long-tailed hydrographs on which to compute a decay slope. These concepts include how surface water and groundwater interact to form statistical properties of \eqn{N}-day-over-day declines. Asquith and Knight propose that \eqn{G_b}-factors implicitly are related to magnitudes of one-day-over-day decay \eqn{\Psi} existing well within the right-tail of the distribution. Thus, amongst the many computed statistics by the \pkg{akqdecay} package, both a \eqn{G_f(F;\Theta)}-factor is computed based on a nonexceedance probability \eqn{F} for \eqn{F \in [0,1]} and probability distribution parameters \eqn{\Theta} and an empirical (nonparametric) \eqn{G_f(F; Z)} for a data sample \eqn{Z} of size \eqn{N}.

The function \code{\link{akqdecay}} is the central entry-point into this package, and this function provides the computational basis for the streamflow changes. The \code{\link{akqdecay}} function generates Asquith--Knight discharge decay analyses. Such analyses are a complex \R data structures and represent the core deliverables the package. These data structures contain the distributional properties of streamflow changes as imparted by the median, L-moments (succinct summary statistics), and special quantiles of both (1) a fitted distribution and (2) the empirical distribution. Special attention is made in order to make period-of-record, annual, and decadal aggregations simultaneously available.

It is useful, though a little advanced in \R language syntax, to succinctly show a ``comprehensive'' regional study of the distributional properties of streamflow recession. Minimalist code showing a framework for batch processing seeking (1) lag one-day as well as (2) decreasing one-day-over-day streamflow (see arguments for \code{\link{akqdecay}}) for six selected USGS Tennessee streamgages on the Wolf River is shown in the snippet below:
\preformatted{
  wolfsites <- c("07030392", "07030500", "07031650",
                 "07031660", "07031700", "07031740")  # USGS streamgages
  wolfdv <- new.env(); akwolf <- new.env() # create environments
  fdwolf <- new.env() # which are very handy for large table storage
  fill_dvenv(wolfsites,      envir=wolfdv)  # wolfdv is FILLED!
  fill_akqenv( dvenv=wolfdv, envir=akwolf)  # akwolf is FILLED!
  fill_tfdcenv(dvenv=wolfdv, envir=fdwolf)  # fdwolf is FILLED!

  rmsites <- union(akq_na(wolfdv),akq_na(akwolf))  # check for degenerates
  akq_rm(wolfdv, rmsites); akq_rm(akwolf, rmsites) # remove them
  wolfsites <- ls(DV) # reset the master list of sites
  # Though for this example, no Wolf River sites are removed.

  SU <- akq_summary(    akwolf); print(head(SU)) # basic summary table
  GT <- akq_global(     akwolf); print(head(GT)) # global statistics
  CN <- akq_counts(     akwolf); print(head(CN)) # table of counts
  LM <- akq_lmom(       akwolf); print(head(LM)) # L-moments for POR
  LY <- akq_lmom_year(  akwolf); print(head(LY)) # L-moments by year
  LD <- akq_lmom_decade(akwolf); print(head(LD)) # L-moments by decade
  ST <- akq_table(      akwolf)  # continuous raw table for all streamgages
        akq_scibase(    akwolf)  # dump to plain-text files of above tables
}
The daily values are retrieved (internally using \code{\link{dvget}}) and stored by streamgage identification number (\code{wolfsites}) in the user-named \R \code{environment} (named \code{wolfdv} for the example). Asquith--Knight discharge decay analyses are computed for each streamgage and stored in \code{akwolf}. Use of environments as a core of multi-site processing by \pkg{akqdecay} is intended in this introductory example to emphasize the generalization of Asquith--Knight analyses for an arbitrary number of streamgages. (Be aware that the leading zero in very many USGS streamgage identification numbers causes a headache for \R [who drops the leading zero in numbers!], and as a result these \emph{must} by stored as character strings or carefully use the \code{\link{gsid2str}} function.)  Finally, trend analysis on the ordinates of the flow-duration curves (\code{\link{fdctrend}}) by year and by streamgage are stored in \code{fdwolf}; users are directed to the \bold{Examples} of \code{\link{fill_tfdcenv}} on how to interact and visualize this analysis.

The tables so stored in \code{akwolf} are possibly not immediately apparent for many users and even those with substantial \R understanding. Therefore, several utility or data-extraction functions are provided to help end users (including the authors). These are \code{\link{akq_summary}}, \code{\link{akq_counts}}, and \code{\link{akq_lmom}} (other analogs are \code{\link{akq_lmom_yeardecade}}, \code{\link{akq_lmom_year}}, and \code{\link{akq_lmom_decade}}). The extracted data are in the from of \R \code{data.frame}'s; these data frames can be conveniently written by the utility function \code{\link{akq_write}} to the user's file system. An ensemble of plain-text output files of the tables from these extraction functions can be produced by \code{\link{akq_scibase}}, which is named for the contemporaneous (\emph{c.}2017) distribution mechanism of certain USGS datasets. The so-called \dQuote{site file} information for the sites can be acquired and emplaced in a spatial data object of the \pkg{sp} package using the function \code{\link{sites_to_SpatialPointsDataFrame}}.

The L-moments (see package \pkg{lmomco}, and Asquith, 2011, 2014) succinctly capture  metrics of distributional geometry. In massive-scale data processing, there might be opportunties to investigate relations between L-moments and underlying surface water and groundwater interactions contributing to the such distributional properties of streamflow hydrograph recession limbs. The \pkg{akqdecay} package is setup, by default, to compute on decreasing days, and these results are in variables \code{LM} (period of record, all decreasing day-over-day daily values), \code{LY} (subsetted by calendar year), and \code{LD} (subsetted by decade).

Though advanced statistical concepts are involved (Asquith, 2011a,b), the relation between L-skew (\eqn{\tau_3}) and L-kurtosis (\eqn{\tau_4}) by year and again by decade (e.g. \dQuote{2000} decade is defined for 01/01/2000 through 12/31/2009) for the Wolf River data can be shown by the following code:
\preformatted{
  lmomco::plotlmrdia(lmomco::lmrdia(), xlim=c(-0.05, 0.7), ylim=c(-0.1,0.6),
                     nopoints=TRUE)
  points(LY$T3, LY$T4, lwd=0.8, col=1, cex=0.8)               # by year
  points(LD$T3, LD$T4, lwd=1.1, col=4, pch=22, bg=8, cex=1.2) # by decade
  points(LM$T3, LM$T4, lwd=1.1, col=1, pch=21, bg=5, cex=1.5) # whole record
  mtext("What distributional form (type) is acceptable to these data?")
}
}
\references{
Asquith, W.H., 2011a, Distributional analysis with L-moment statistics using the R environment for statistical computing: Ph.D. dissertation, Texas Tech University, accessed on March 15, 2017 at \url{https://ttu-ir.tdl.org/ttu-ir/handle/2346/ETD-TTU-2011-05-1319}.

Asquith, W.H., 2011b, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace, [print-on-demand], ISBN 978--146350841--8, [reprinting of Asquith (2011a) with errata]

Asquith, W.H., 2014, Parameter estimation for the 4-parameter asymmetric exponential power distribution by the method of L-moments using R: Computational Statistics and Data Analysis, v. 71, pp. 955--970, \url{http://dx.doi.org/10.1016/j.csda.2012.12.013}

Asquith, W.H., 2018, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.3.2 at \url{https://cran.r-project.org/package=lmomco}.

Bingham, R.H., 1982, Low-flow characteristics of Alabama streams, USGS Water Supply Paper 2083, 27 p., \url{http://pubs.er.usgs.gov/publication/wsp2083}

Bingham, R.H., 1986, Regionalization of low-flow characteristics of Tennessee streams: U.S. Geological Survey Water-Resources Investigations Report 85--4191, 63 p., \url{http://pubs.usgs.gov/wri/wri85-4191/}
}
\author{ W.H. Asquith, \email{wasquith@usgs.gov}; R.R. Knight, \email{rrknight@usgs.gov} }
\seealso{
   \code{\link{akqdecay}}, # *** The very core of this package, and it will be loved before the end. ***\cr
   \code{\link{akq_counts}}, \code{\link{akq_lmom}},
   \code{\link{akq_summary}}, \code{\link{akq_table}},
   \code{\link{akq_write}}, \code{\link{akq_scibase}}, # data extraction\cr
   \code{\link{dvget}}, \code{\link{fill_akqenv}}, \code{\link{fill_dvenv}}, \code{\link{fill_tfdcenv}} # data and batch processing functions\cr
    \code{\link{gfredo}}, \code{\link{lmrdf2gfactor}}, \code{\link{gsid2str}} # manipulation functions\cr
    \code{\link{fdctrend}}, \code{\link{visFDCtrend}} # trend in the flow-duration curve\cr
    \code{\link{akq_summaryfilter}}, \code{\link{akq_na}}, \code{\link{akq_rm}} # utilities to refine processing\cr
    \code{\link{sites_to_SpatialPointsDataFrame}} # see the sp library
}
