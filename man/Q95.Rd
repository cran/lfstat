\name{Qxx}
\alias{Q95}
\alias{Qxx}
\alias{Q70}
\alias{Q90}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Qxx, Q95, Q90, Q70
}
\description{
Calculates the quantiles of an object of class \code{'lfobj'}.
}
\usage{
Qxx(lfobj, Qxx, year = "any", monthly = FALSE, yearly = FALSE,
breakdays = NULL, na.rm = TRUE)

Q95(lfobj, year = "any", monthly = FALSE, yearly = FALSE,
breakdays = NULL, na.rm = TRUE)

Q90(lfobj, year = "any", monthly = FALSE, yearly = FALSE,
breakdays = NULL, na.rm = TRUE)

Q70(lfobj, year = "any", monthly = FALSE, yearly = FALSE,
breakdays = NULL, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class \code{'lfobj'}
  }
  \item{Qxx}{The quantile to calculate, e.g. 70 would refer to Q70}
  \item{year}{
The year for which the Q95 should be computed. If \code{hyearstart != 1} the Q95 is calculated for the hydrological year, \code{year = "any"} means the whole series should be taken.
}
  \item{monthly}{
logical - Should the Q95 be calculated separately for every month?.
}
  \item{yearly}{
logical - If TRUE,  the Q95 is calculated for each hydrological year separately.
}
 \item{breakdays}{
A vector of break days if the Q95 should be calculated for different seasons.
}
\item{na.rm}{Should NAs be ignored?}
}
\details{
If \code{breakdays} is a single day, e.g. "01/06", the start of the hydrological year is taken as the second break day. If more than two seasons are to be specified, a vector of all break days is needed.
}

\value{
A length one vector giving the Q95 for the whole series or the specified
year. If yearly is true, a vector of the annual Q95s is returned. If
break days are specified, the values are separated per season.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{MAM}}}

\examples{
data(ngaruroro)
Q95(ngaruroro)
Q95(ngaruroro, breakdays = c("01/11","01/05"))
Q95(ngaruroro, year = 1991)
# Calculate Q99
Qxx(ngaruroro, Qxx = 99)
}
