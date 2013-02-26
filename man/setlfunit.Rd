\name{setlfunit}
\alias{setlfunit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define the unit to use in lf-plots
}
\description{
Sets the option for the unit in plots.
}
\usage{
setlfunit(string = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{String of the unit}
}

\details{
The unit string should be readable for the R-function
\code{\link{expression}}, for common units see example below. 
}

\section{Warning}{
  No calculation on data is done by setting this string.
  }

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}

\examples{
data(ngaruroro)
#Default: no unit
bfplot(ngaruroro, year = 1991)

#The plot does not change, just the y-label does!
setlfunit("m^3/s")
bfplot(ngaruroro,year = 1991)

#Some possible labels:
setlfunit("m^3/s")
setlfunit("m^{3}*s^{-1}")
setlfunit("scriptscriptstyle(frac(m^3,s))")
setlfunit("l/s")
setlfunit("l*s^{-1}")
setlfunit("scriptscriptstyle(frac(l,s))")
setlfunit("m^3/s/km^2")
setlfunit("m^3*s^{-1}*km^{-2}")
setlfunit("scriptscriptstyle(frac(m^3,s\%.\%km^2))")
setlfunit("l/s/km^2")
setlfunit("l*s^{-1}*km^{-2}")
setlfunit("scriptscriptstyle(frac(l,s\%.\%km^2))")
}

