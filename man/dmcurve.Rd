\name{dmcurve}
\alias{dmcurve}
\title{
Double Mass Curve
}
\description{
Calculates the double mass curve of two object of class \code{'lfobj'}.
}
\usage{

dmcurve(x, y, year = "any", namex = substitute(x), namey = substitute(y),
    na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{"lfobj"}
  }
  \item{y}{An object of class \code{"lfobj"}}

  \item{year}{
The year for which the double mass curve should be calculated
}
  \item{namex}{
character - Label of the x-Axis in the double mass curve
}
  \item{namey}{
character - Label of the y-Axis in the double mass curve
}
 \item{na.rm}{
Remove NAs?
}
}

\value{No return value, called for side effects (plotting).}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}

\author{
Daniel Koffler and Gregor Laaha
}

\examples{
data(ngaruroro)
n1 <- subset(ngaruroro, year \%in\% 1985:1989)
n2 <- subset(ngaruroro, year \%in\% 1990:1995)
dmcurve(n1,n2, namex = "'Ngaruroro 1985 - 1989'", namey = "'Ngaruroro 1990
- 1995'")
}
