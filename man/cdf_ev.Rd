\name{reversing}
\alias{reversing}
\alias{cdf_ev}
\alias{pel_ev}
\alias{qua_ev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reversed functions for several Extreme Value Distributions}
\description{
As several Extreme Value distributions are parameterised for high extreme values, reversed functions for minima (e.g. low flow statistics) are derived. Reversing is done by fitting to the negated data (\code{-x}), subtracting probabilities from one (\code{1 - f}) and computing the negated probabilities.}
\usage{
cdf_ev(distribution, x, para)
pel_ev(distribution, lmom, ...)
qua_ev(distribution, f, para)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distribution}{character vector of length one containing the name of the distribution. The family of the chosen distribution must be supported by the package lmom. See \code{\link{lmom}}. For example \code{distribution = "gev"} directly uses the functions from package lmom, whereas \code{distribution = "gevR"} performs reversing.}
  \item{x}{Vector of quantiles.}
  \item{f}{Vector of probabilities.}
  \item{para}{Numeric vector containing the parameters of the distribution, in the order zeta, beta, delta (location, scale, shape).}
 \item{lmom}{Numeric vector containing the L-moments of the distribution or of a data sample. E.g. as returned by \code{samlmu(x)}.}
 \item{...}{parameters like \code{bound}, passed on to the estimating function. E.g. in case of \code{dist = 'wei'} to \code{pelwei}.}
}
\value{cdf_ev gives the distribution function; qua_ev gives the quantile function.}
\seealso{
\code{\link{lmom}}, \code{\link[lmom]{cdfgev}}, \code{\link[lmom]{quagev}}, \code{\link[lmom]{pelgev}}.
}
\examples{
data("ngaruroro")
ng <- as.xts(ngaruroro)
minima <- as.vector(apply.yearly(ng$discharge, min, na.rm = TRUE))

# Weibull distribution and reversed GEV give the same results
distr <- "wei"
qua_ev(distr, seq(0, 1, 0.1), para = pel_ev(distr, samlmu(minima)))

distr <- "gevR"
qua_ev(distr, seq(0, 1, 0.1), para = pel_ev(distr, samlmu(minima)))
}

\keyword{low-flow}
\keyword{fitting}
