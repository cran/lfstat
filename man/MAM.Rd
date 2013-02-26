\name{MAM}
\alias{MAM}

\title{
Mean Annual Minimum
}
\description{
Computes the Mean Annual Minimum (MAM-n) for any given n.
}
\usage{
MAM(lfobj, n = 7, year = "any",breakdays = NULL, yearly = FALSE)
}

\arguments{
    \item{lfobj}{An object of class "lfobj"
    }
    \item{n}{Mean Annual minimum for n-days, e.g. n=7 computes MAM7}
  \item{year}{
The year for which the BFI should be computed. If hyearstart != 1 the
    BFI is calculated for the hydrological year! "any" means the whole
    series should be taken. If a vector of years is given, all this
    years are included in the calculation.
}
  \item{breakdays}{
A vector of breakdays if the BFI should be calculated for diffenent seasons. 
}
  \item{yearly}{
If TRUE,  the BFI is calculated for each hydrological year seperately. 
}
}

\details{
If breakdays is a single day, e.g. "01/06", the start of the hydrological year is taken as the second breakday. If more than two seasons are to be specified, a vector of all breakdays is needed.
}

\value{A length one vector giving the BFI for the whole series or the
  specified year. If yearly is true, a vector of the annual BFIs is
  returned. If breakdays are specified, separated values for every
  season are given.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\note{
The annual minima can be calculated by setting n=1 and yearly = TRUE.
}

\section{Warning }{
At the moment there is no check for seasonal overlap. E.g. The MAM7 of
1991 and 1992 could take the same days for calculation if the are in
n/2-days range. This problem could be avoided by choosing a
"meaningfull" hyearstart and breakdays, usually dates out of the low
flow seasons.
}

\seealso{
  \code{\link{meanflow}},\code{\link{Q95}}
}
\examples{
data(ngaruroro)
MAM(ngaruroro)
MAM(ngaruroro, n=1) #Mean annual minimum
MAM(ngaruroro, year = c(1991,1995)) #Taking values from 1991 and 1995
MAM(ngaruroro, year = 1991:1995) #Taking values from 1991 to 1995 (1991,1992,...,1995)
MAM(ngaruroro, breakdays = c("01/11","01/05"))
MAM(ngaruroro, year = 1991)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MAM}
\keyword{mean annual minimum }% __ONLY ONE__ keyword per line
