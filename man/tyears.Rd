\name{tyears}
\alias{tyears}

\title{
Calculate Low-Flow Quantiles for given Return Periods
}

\description{Fits an extreme value distribution using \var{L}-moments to the minima of a time series of discharges and subsequently estimates quantiles (the so called \var{T}-years event) for given return periods. In the presence of zero flow observations a mixed distribution is fitted.
}
\usage{
tyears(lfobj, event = 1/probs, probs = 0.01,
       dist, check = TRUE, zeta = zetawei, zetawei = NULL,
       plot = TRUE, col = 1, log = TRUE, legend = TRUE,
       rp.axis = "top", rp.lab = "Return period",
       freq.axis = TRUE,
       freq.lab = expression(paste("Frequency " *(italic(F)),
                                   " = Non-Exceedance Probability P ",
                                   (italic(X) <= italic(x)))),
       xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
       ylab = "Quantile",
       hyearstart = hyear_start(lfobj),
       n = NULL)
}

\arguments{
  \item{lfobj}{An object of class lfobj or an object which can be coerced to class xts. Either with a single column or with a column named 'discharge'.}
  \item{event}{numeric vector specifying the return periods. E.g. \code{event = 100} will yield the 100 years extreme low flow event.}
  \item{probs}{Alternate way to specify the return period of the event.}
  \item{dist}{A character vector of distributions to fit. Basically all distributions provided by Hosking's \code{\link[lmom]{lmom-package}} and their reversed counterparts can be chosen.}
  \item{check}{logical, should \code{\link{check_distribution}} get called?}
    \item{zeta}{numeric vector of length one for manually setting a lower bound. Only a few distributions allow for a lower bound, namely \code{'gpa'}, \code{'ln3'}, \code{'wak'} and \code{'wei'}. The default value of \code{NULL} results in not bounding the distribution, therefore the parameter \code{zeta} is estimated.}
  \item{zetawei}{same as \code{zeta}}
  \item{plot}{logical. If \code{TRUE}, sample observations as well as estimated quantile functions are plotted.}
  \item{col}{numeric or character vector of length one or as long as \code{dist}, specifying the color used for plotting.}
  \item{log}{logical. If \code{TRUE} probabilities will be plotted on a double logarithmic scale.}
  \item{legend}{logical, should a legend be added to the plot?}
  \item{rp.axis}{vector of length one, specifying if and how an additional scale bar for the return periods is drawn. Possible choices are \code{'bottom'}, \code{'top'} and \code{'none'}. Alternatively, the position of the scale bar can be specified as an real number between 0 and 1, indicating the y-position of the legend.}
  \item{rp.lab}{character vector, text above the scale bar for return periods}
  \item{freq.axis}{logical, should an additional abscissa showing the probabilities be drawn on top of the plot?}
  \item{freq.lab}{character vector, text above the probability axis}
  \item{xlab}{character vector, a label for the x axis}
  \item{ylab}{character vector, a label for the y axis}
  \item{hyearstart}{vector of length one, providing the start of the hydrological year. This is evaluated by \code{\link{water_year}}. The default is, to retrieve the values stored in the attributes of the \code{lfobj}.}
  \item{n}{Argument 'n' is deprecated and ignored. To apply a moving average, do it prior to calling 'tyears'. See section Examples.}
}


\details{
This function is vectorised over \code{dist} and \code{event}.


According to paragraph 7.4.2 of the WMO manual, special care has to be taken in the presence of zero flow observations. A cdf called G(x) is fitted to the non-zero values of the original  time series

If a distribution is fitted which allows for finite lower bound (\code{zeta}), and \code{zeta} is estimated being negative, estimation is repeated constraining \code{zeta = 0}. If this behavior is not desired, the parameter \code{zeta} has to be set explicitly.
}

\value{
  An object of class evfit, see \code{\link{evfit}}.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}

\seealso{
There are methods for printing summarizing objects of class evfit.

\code{\link{evfit}}
}

\examples{
data("ngaruroro")
ng <- subset(ngaruroro, hyear \%in\% 1964:2000)

# vector of return periods
rp <- c(1.5, 5, 10, 100)

# Fitting some distributions for the low flows (annual minima)
# and estimating the quantile for arbitrary return periods
y <- tyears(ng, dist = c("gum", "wei", "ln3", "pe3"), event = rp,
            plot = FALSE)

# print()ing the object shows just the return periods
y

# but y is actually a list
str(y)

# there is a summary method, returning L-moments and estimated parameters
summary(y)

plot(y)


# fitting just one distribution, with annotated quantiles
z <- tyears(ng, dist = c("gevR"), event = rp)
rpline(y, return.period = rp, suffix = c("a", "m\u00B3"))


# applying a moving average before fitting
ng2 <- ng
ng2$flow <- ma(ng2$flow, n = 4)
tyears(ng2, dist = c("gum", "wei", "ln3", "pe3"), event = rp,
       plot = FALSE)
}

\keyword{low-flow}
\keyword{fitting}
