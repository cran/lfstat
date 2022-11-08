\name{vary_threshold}
\alias{vary_threshold}

\title{Create varying thresholds }
\description{Helper function to easily create a daily, weekly, monthly or seasonal varying threshold.}
\usage{
vary_threshold(x, varying = "constant",
               fun = function(x) quantile(x, probs = 0.05, na.rm = TRUE), ...)
}

\arguments{
  \item{x}{an object which can be coerced to class \code{'xts'}. Either with a single column or with a column named 'discharge'.}
  \item{varying}{if \code{varying} is a character vector of length one, values of "constant", "daily", "weekly" and "monthly" are allowed. If a vector of class POSIX is provided, a seasonal varying threshold is computed, where the times provided define the start of the season. Only the day of the year is taken, the year itself doesn't have a meaning.}
  \item{fun}{a function accepting a single argument and returning either a vector of length one or a vector as long as \code{x}.}
  \item{\dots}{additional arguments, passed on to \code{fun}}
}


\value{a vector as long as \code{x}.}
\examples{
data(ngaruroro)
ng <- as.xts(ngaruroro)["1983::1985", ]
r <- find_droughts(ng, varying = "monthly")
plot(r)

thr1 <- vary_threshold(ng, varying = "weekly", fun = mean, na.rm = TRUE)
plot(thr1)

thr2 <- vary_threshold(ng, varying = "monthly", fun = mean, na.rm = TRUE)
lines(thr2, col = 2)
}

\keyword{low-flow}
