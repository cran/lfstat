\name{evquantile}
\alias{evquantile}
\title{Estimating populations quantiles of extreme values}
\description{Computes population quantiles for given return periods. Estimation is done using \var{L}-moments.}
\usage{
evquantile(fit, return.period = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{object of class \code{evfit}, possibly created with \code{evfit()}.}
  \item{return.period}{numeric vector of return periods}
}
\details{This function is vectorized over \code{return.period}.}
\value{A matrix containing the low-flow quantiles, with rows corresponding to return periods columns to distributions.}
\examples{
data("ngaruroro")

# using tyears is a fast way to produce an object of class evfit
y <- tyears(ngaruroro, dist = "wei", event = 100, plot = TRUE)

# computing quantiles for given return periods
rp <- c(1.42, 5, 10)
evquantile(y, return.period = rp)
rpline(y, return.period = rp, suffix = c("a", "m\u00B3"))
}

\keyword{low-flow}
\keyword{fitting}
