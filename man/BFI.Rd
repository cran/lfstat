\name{BFI}
\alias{BFI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Base Flow Index
}
\description{
Calculates the base flow index of an object of class \code{'lfobj'}.
}
\usage{
BFI(lfobj, year = "any",breakdays = NULL, yearly = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class \code{'lfobj'}
}
  \item{year}{
The year for which the BFI should be computed. If \code{hyearstart != 1} the BFI is calculated for the hydrological, \code{year = "any"} means the whole series should be taken.
}
  \item{breakdays}{
A vector of break days if the BFI should be calculated for different seasons.
}
  \item{yearly}{
If TRUE, the BFI is calculated for each hydrological year separately.
}
}
\details{
If \code{'breakdays'} is a single day, e.g. "01/06", the start of the hydrological year is taken as the second break day. If more than two seasons are to be specified, a vector of all break days is needed.
}

\value{
A length one vector giving the base flow index for the whole series or the specified
year. If yearly is true, a vector of the annual base flow indices is returned. If
break days are specified, the values are separated per season.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{bfplot}}}

\examples{
data(ngaruroro)
BFI(ngaruroro)
BFI(ngaruroro, breakdays = c("01/11","01/05"))
BFI(ngaruroro, year = 1991)
bfplot(ngaruroro, year = 1991)
}
\keyword{Base flow index}
