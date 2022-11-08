\name{check_distribution}
\alias{check_distribution}
\title{Checks if a Distribution is suited}
\description{Most distributions are used for modelling either minima or maxima. Sometimes a better fit can be achieved by reversing the distribution. This functions helps to decide if the reversed distribution is advisable.}
\usage{
check_distribution(extreme = c("minimum", "maximum"), distribution,
                   def = list(minimum = c(),
                              maximum = c("gev")))
}

\arguments{
  \item{extreme}{character vector, describing the kind of extreme value to be fitted. Either \code{'minimum'} or \code{'maximum'}.}
  \item{distribution}{character vector of length one. Distribution chosen by the user.}
  \item{def}{a list of length two, containing the elements \code{'minimum'} and \code{'maximum'}.}
}

\value{a character vector as long as \code{distribution} containing the optimal choice for the given \code{distribution}s under the constraints of \code{def}.
}

\examples{
# Using the Weibull distribution for minimum values is a good choice
check_distribution(extreme = "minimum", distribution = "wei")

# ... whereas the GEV is meant for maxima.
# Therefore the reversed distribution is suggested.
check_distribution(extreme = "minimum", distribution = "gev")
}

\keyword{low-flow}
\keyword{distribution}
