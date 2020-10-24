\encoding{utf8}
\name{lmrdf2gfactor}
\alias{lmrdf2gfactor}
\title{Convert L-moment Data Frame from Asquith--Knight Discharge Decay Analyses to Their Probabilistic Gfactor}
\description{
Internally, \code{\link{akqdecay}} generates an \R \code{data.frame} (actually several) having columns named \code{L1}, \code{L2}, \code{T3}, \code{T4}, \code{T5}, \code{T6} for the first six L-moments (Asquith, 2018) of the \eqn{N}-day-over-day change values. This data frame is then augmented by a call to this function (\code{lmrdf2gfactor}) to compute the quantile for a given nonexceedance probability (\eqn{F \in [0,1]}) and distribution type as supported by the \pkg{lmomco} package.

Let us suppose that some 4,600 one-day-over-day of streamflow declines, expressed in units of days per base-10 logarithmic cycle (just \dQuote{days}), have the following L-moments: \eqn{\lambda_1 = 73} days (mean), \eqn{\lambda_2 = 38} days (L-scale), and \eqn{\tau_3 = 0.4} dimensionless (L-skew). Let us suppose that the three-parameter generalized Pareto distribution (GPA) is applicable. The parameters of this distribution when fit to the L-moments are \eqn{\xi = 2.429}, \eqn{\alpha = 60.490}, and \eqn{\kappa = -0.1429}. If the 90th percentile (\eqn{F=0.90} nonexceedance probability) is reflective of long-term streamflow persistence (perhaps caused by sustained groundwater contributions) and is a suitable metric for Gfactor (\eqn{G_f}), this value thus is
\deqn{G_f = x_{\mathrm{gpa}}(0.9; \Theta) = 167\ \mathrm{days}\mbox{,}}
where the requisite computations using the quantile function of the chosen distribution (\eqn{x_{\mathrm{gpa}}}) and parameters (\eqn{\Theta = (\xi, \alpha, \kappa)}) using the \pkg{lmomco} package are
\preformatted{
  para <- lmomco::pargpa(lmomco::vec2lmom(c(73, 38, 0.4)))
  GF   <- lmomco::quagpa(0.9, para)
  message("# Gfactor=", round(GF, digits=1), " days (90th percentile estimate)")
  # Gfactor=167.4 days (90th percentile estimate)
}
}
\usage{
lmrdf2gfactor(df, f, type="gpa", ...)
}
\arguments{
  \item{df}{An \R \code{data.frame} with six named columns of L-moments: \code{L1}, \code{L2}, \code{T3}, \code{T4}, \code{T5}, and \code{T6} for the L-moments \eqn{\lambda_1}, \eqn{\lambda_2}, \eqn{\tau_3}, \eqn{\tau_4}, \eqn{\tau_5}, and \eqn{\tau_6}, respectively;}
  \item{f}{Nonexceedance probability (\eqn{F \in [0,1]}), and the default is the 90th percentile;}
  \item{type}{Distribution type of the \pkg{lmomco} package to estimate the \eqn{G_f} factor for the probability \code{f}.  If the type is \code{NA}, then the computation of the fitted distribution is short circuited; this is useful for speed during testing (for instance). The default is the three-parameter generalized Pareto; and}
  \item{...}{Additional arguments to pass to control functions \code{lmom2par} and \code{qlmomco} of the \pkg{lmomco} package.}
}
\value{
An \R \code{vector} of quantiles from the probability distribution with a length of the number of rows in the \code{data.frame} represented by \code{df}.  The first six L-moments are hardwired for computation if the sample size is sufficient for their respective computation, though in general practice a \eqn{p}-parameter distribution uses only the first \eqn{p} L-moments for parameter estimation. As a result, a three-parameter distribution, such as the generalized Pareto (GPA), requires only the first three L-moments for parameter estimation.
}
\note{
This function is intended as a helper function for the internals of \code{\link{akqdecay}}, but it is generic enough to justify an user-level interface. The \pkg{akqdecay} package is intended for large data mining computations. Towards this goal, a special trap for incomplete L-moments is made for protection against failure within the \pkg{lmomco} package. If the L-moments do not test as valid by \code{lmomco::}\code{are.lmom.valid()} or if either L-skew (\eqn{\tau_3}) or L-kurtosis (\eqn{\tau_4}) is \code{NA}, then the Gfactor is computed as \code{NA}. This is mentioned because by design, it is easy to leak missing L-skew into \pkg{lmomco}'s distribution fitting and breakage occurs there and not within \pkg{akqdecay} and that \code{\link{akqdecay}} does this protection for period-of-record statistics because \code{lmrdf2gfactor} is not called in that situation.

The GPA distribution is the default. The quantile function of the distribution is
\deqn{x(F) = \xi + \alpha[1-(1-F)^\kappa]/\kappa\mbox{,}}
for \eqn{\kappa \ne 0}, and
\deqn{x(F) = \xi - \alpha\log(1-F)\mbox{,}}
for \eqn{\kappa = 0}, where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F}, \eqn{\xi} is a location parameter, \eqn{\alpha} (\eqn{\alpha > 0}) is a scale parameter, and \eqn{\kappa} (\eqn{\kappa > -1}) is a shape parameter. The cumulative distribution function is
\deqn{F(x) = 1 - \mathrm{exp}(-Y) \mbox{, and}}
the probability density function is
\deqn{f(x) = \alpha^{-1} \exp[-(1-\kappa)Y] \mbox{,}}
where \eqn{Y} is
\deqn{Y = -\kappa^{-1}\log[1 - (\kappa(x-\xi)/\alpha)]\mbox{,}}
for \eqn{\kappa \ne 0} and
\deqn{Y = (x-\xi)/\alpha\mbox{,}}
for \eqn{\kappa = 0}.  The L-moments in terms of the parameters are
\deqn{\lambda_1 = \xi + \alpha/(\kappa+1) \mbox{,}}
\deqn{\lambda_2 = \alpha/[(\kappa+2)(\kappa+1)] \mbox{,}}
\deqn{\tau_3 = (1-\kappa)/(\kappa+3) \mbox{, and}}
\deqn{\tau_4 = (1-\kappa)(2-\kappa)/[(\kappa+4)(\kappa+3)] \mbox{.}}
}
\author{ W.H. Asquith}
\references{
Asquith, W.H., 2011a, Distributional analysis with L-moment statistics using the R environment for statistical computing: Ph.D. dissertation, Texas Tech University.

Asquith, W.H., 2011b, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace, [print-on-demand], ISBN 978--146350841--8, \url{https://www.amazon.com/dp/1463508417}. [reprinting of Asquith (2011a) with errata]

Asquith, W.H., 2018, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.3.2 at \url{https://cran.r-project.org/package=lmomco}.
}
\seealso{\code{\link{akqdecay}}
}
\examples{
# See Examples under akqdecay and elsewhere in this documentation.
}
\keyword{Gfactor}
\keyword{L-moments}
