\name{fill_na}
\alias{fill_na}

\title{
Interpolation NA values in a vector
}
\description{This function is a tiny wrapper around \code{\link{approx}} which allows to contain the maximum number of NA values in a row that will be filled by interpolation. This is useful to obtain regular time series.}

\usage{
fill_na(x, max.len = Inf, ...)
}
\arguments{
  \item{x}{a vector, possibly containing NA values}
  \item{max.len}{an integer vector of length one, constraining the number of of consecutive NA observations which will get replaced with interpolated values}
  \item{\ldots}{further arguments, passed on to \code{\link{approx}}.}
}
\value{a vector}

\seealso{\code{\link{approx}}, \code{\link{na.approx}}}
\examples{
x <- 1:20
x[c(2, 3, 6, 11:15)] <- NA
fill_na(x, max.len = 2)
}

\keyword{ utilities }
\keyword{ misc }
