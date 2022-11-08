\name{ma}
\alias{ma}
\title{Simple Moving Average}
\description{
Smoothing a time series with moving averages using the \code{filter} function.
}
\usage{
ma(x, n, sides = "past")
}
\arguments{
  \item{x}{
numeric vector to be smoothed
}
  \item{n}{
numeric vector of length one determining the width of the smoothing window
}
  \item{sides}{
one of \code{'past'}, \code{'center'} or \code{'future'} indicating the side of the filter.)
}
}

\value{
a vector as long as x, but smoothed. Possibly with NAs.}

\seealso{
\code{\link{filter}}
}
\examples{
ma(1:10, n = 3, sides = 2)    # centred around lag 0
ma(1:10, n = 3)               # past values
}

\keyword{ misc }

