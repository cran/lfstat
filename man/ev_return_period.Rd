\name{ev_return_period}
\alias{ev_return_period}

\title{
Estimate the return period for given quantiles
}
\description{
For discharges of interest, estimate the corresponding return period.
}
\usage{
ev_return_period(x, fit)
}
\arguments{
  \item{x}{numeric vector containing the quantiles}
  \item{fit}{object of class \code{evfit} describing the underlying distribution, possibly created with \code{evfit()}.}
}
\value{a numeric vector of return periods.}

\seealso{
\code{\link{evfit}}
}
\examples{
data("ngaruroro")
ng <- as.xts(ngaruroro)

# yearly minima
minima <- apply.yearly(ng$discharge, min, na.rm = TRUE)

# fit a Weibull distribution
fit <- evfit(x = as.vector(minima), distribution = "wei")

# compute return periods
minima$rp <- round(ev_return_period(minima, fit), 2)

print(minima)
plot(discharge ~ rp, data = minima,
     xlab = "Flow in m^3/s", ylab = "Return period in years")
}

\keyword{low-flow}
\keyword{fitting}
