\name{BFI}
\alias{BFI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Base Flow Index
}
\description{
Calculates the base flow index of an object of class 'lfobj'.
}
\usage{
BFI(lfobj, year = "any",breakdays = NULL, yearly = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"
}
  \item{year}{
The year for which the BFI should be computed. If hyearstart != 1 the BFI is calculated for the hydrological year "any" means the whole series should be taken.
}
  \item{breakdays}{
A vector of breakdays if the BFI should be calculated for different seasons.
}
  \item{yearly}{
If TRUE,  the BFI is calculated for each hydrological year separately.
}
}
\details{
If breakdays is a single day, e.g. "01/06", the start of the hydrological year is taken as the second breakday. If more than two seasons are to be specified, a vector of all breakdays is needed.
}

\value{
A length one vector giving the BFI for the whole series or the specified
year. If yearly is true, a vector of the annual BFIs is returned. If
breakdays are specified, the values are separated per season.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
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
