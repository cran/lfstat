\name{evfit}
\alias{evfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit an extreme value distribution to observations}
\description{Fits an extreme value distribution using \var{L}-moments to the values provided. In the presence of zero flow observations a mixed distribution is fitted.}
\usage{
evfit(x, distribution, zeta = NULL, check = TRUE,
      extreme = c("minimum", "maximum"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector. Data which is an extreme value distribution is fitted to. }
  \item{distribution}{A character vector of distributions to fit. Basically all distributions provided by Hosking's \code{\link[lmom]{lmom-package}} and their reversed counterparts can be chosen. See \code{\link{reversing}}.}
  \item{zeta}{numeric vector of length one for manually setting a lower bound. Only a few distributions allow for a lower bound, namely \code{'gpa'}, \code{'ln3'}, \code{'wak'} and \code{'wei'}. The default value of \code{NULL} results in not bounding the distribution, therefore the parameter \code{zeta} is estimated.}
  \item{check}{logical, should \code{\link{check_distribution}} get called?}
  \item{extreme}{character vector of length one. Can be either \code{'minimum'} or \code{'maximum'}. Helps to choose a correct distribution.}
}

\details{This function is vectorized over \code{distribution}.

According to paragraph 7.4.2 of the \acronym{WNO} manual, special care has to be taken in the presence of zero flow observations. A cdf called G(x) is fitted to the non-zero values of the original time series.

If a distribution is fitted which allows for finite lower bound (\code{zeta}), and \code{zeta} is estimated being negative, estimation is repeated constraining \code{zeta = 0}. If this behavior is not desired, the parameter \code{zeta} has to be set explicitly.
}
\value{An object of class \code{'evfit'} containing the \var{L}-moments and the estimated parameters is returned. Objects of class \code{'evfit'} are basically a list with the following elements:

\item{values}{the values \code{x} used for fitting.}
\item{freq.zeros}{a character vector of length one. Frequency of zero flow observations.}
\item{is.censored}{logical, if the censored time was used for fitting.}
\item{parameters}{a list as long as \code{distribution} containing the estimated parameters for each distribution.}
\item{lmom}{sample L-moments of the censored series (only containing non-zero values). }
\item{extreme}{character vector of length one, indicating what kind of extreme value was fitted.}
\item{T_Years_Event}{optional. If quantiles have been computed they a stored in a matrix with return periods in rows and distributions in columns.}
}

\seealso{
There are methods for printing summarizing objects of class \code{'evfit'}.

\code{\link{evfit}}
}

 \examples{
data("ngaruroro")
ng <- as.xts(ngaruroro)
minima <- as.vector(apply.yearly(ng$discharge, min, na.rm = TRUE))
evfit(x = minima, distribution = c("wei", "gevR"),
      extreme = "minimum")
}

\keyword{low-flow}
\keyword{fitting}
