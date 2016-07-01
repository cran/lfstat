\name{rpline}
\alias{rpline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Highlight quantiles/return periods}
\description{Draw a Line in an extreme value plot corresponding to a given return period.}
\usage{
rpline(fit, return.period = NULL, log = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{object of class \code{evfit}, possibly created with \code{evfit()}.}
  \item{return.period}{numeric vector of return periods}
  \item{log}{logical. If \code{TRUE}  it is assumed that probabilities were plotted on a double logarithmic scale.}
  \item{\dots}{other arguments, passed on to \code{\link[lfstat]{trace_value}}}
}
\details{Computes the corresponding quantiles and draws lines and labels.}
\value{This function is used for its side effects}
\examples{
data("ngaruroro")
y <- tyears(ngaruroro, dist = "wei", event = 100, plot = TRUE)
rp <- c(1.42, 5, 10)
rpline(y, return.period = rp, suffix = c("a", "m\u00B3"))
}

\keyword{low-flow}
\keyword{aplot}
