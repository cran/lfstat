\name{dmcurve}
\alias{dmcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Double Mass Curve
}
\description{
Calculates the double mass curve of two object of class 'lfobj'.
}
\usage{

dmcurve(x, y, year = "any", namex = substitute(x), namey = substitute(y), 
    na.rm = TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class "lfobj"
  }
  \item{y}{An object of class "lfobj"}
  
  \item{year}{
The year for which the dmcuve should be calculated
}
  \item{namex}{
character - Label of the x-Axis in the dmcurve 
}
  \item{namey}{
character - Label of the y-Axis in the dmcurve
} 
 \item{na.rm}{
Remove NAs?
}
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
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
