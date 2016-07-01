\name{gringorten}
\alias{gringorten}
\title{
Gringorten Plotting Positions
}
\description{
Computes the Gringorten Plotting position.}
\usage{
gringorten(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector}
}

\value{numeric vector in \code{[0, 1]}, giving the corresponding plotting positions.}

\examples{
y <- rnorm(10)
pp <- gringorten(y)
pp

plot(pp ~ y, ylim = c(0, 1))
}

\keyword{low-flow}
