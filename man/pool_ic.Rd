\name{pooling}
\alias{pooling}
\alias{pool_ic}
\alias{pool_ma}
\alias{pool_sp}
\alias{pool_it}


\title{Pooling Procedures of Low Flow Events}
\description{
Several pooling procedures can be applied to reduce the number of dependent droughts.
}
\usage{
pool_ic(x, tmin = 5, ratio = 0.1)
pool_it(x, tmin = 5)
pool_ma(x, n = 10)
pool_sp(x)
  }

\arguments{
  \item{x}{an object of class \code{deficit}, e.g. as produced by
  \code{\link{find_droughts}}.}
  \item{tmin}{numeric vector of length one interpreted as the number of days between two droughts to be considered independent events. Two droughts are pooled if their inter-event time is less than \code{tmin}.}
  \item{ratio}{numeric vector of length. Specifies the minimum ratio of inter-event volume and precedent drought volume. Two droughts are pooled if the critical \code{ratio} is deceeded.}
  \item{n}{numeric vector of length one determining the width of the smoothing window}
  }
\details{
The inter-event criterion (\code{pool_ic}) pools subsequent drought events if the inter-event time is less than \code{tmin} and the ratio of the drought volume and the inter-event volume is less than a given \code{ratio}. The function \code{pool_it} is simply a wrapper around \code{pool_ic(..., ratio = Inf)}.

Pooling by a moving average (\code{pool_ma}) simply smooths the time series before finding drought events.

Using the Sequent Peak algorithm (\code{pool_sp}), a drought lasts until its cumulative deficit volume is zero again.
}

\value{an object of class \code{deficit} (inherited from \code{xts}), with an
additional column \code{event.orig}.}
%\references{}

\seealso{\code{\link{find_droughts}}, \code{\link{summary.deficit}}}
\examples{
data(ngaruroro)
ng <- as.xts(ngaruroro)
ng <- ng["1986::1990", ]

drought <- find_droughts(ng)


ic <- pool_ic(drought)
summary(ic)

ma <- pool_ma(drought)
summary(ma)

sp <- pool_sp(drought)
summary(sp)
plot(sp)
}

\keyword{low-flow}
