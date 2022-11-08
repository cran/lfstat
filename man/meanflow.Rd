\name{meanflow}
\alias{meanflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean flow
}
\description{
Calculates the mean flow of an object of class \code{'lfobj'}.
}
\usage{
meanflow(lfobj, year = "any", monthly = FALSE, yearly = FALSE,
breakdays = NULL, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class \code{"lfobj"}
}
  \item{year}{
The year for which the mean flow should be computed. If \code{hyearstart != 1} the mean flow is calculated for the hydrological year! "any" means the whole series should be taken.
}
  \item{monthly}{
logical - Should the mean flow be calculated separately for every month?.
}
  \item{yearly}{
logical - If TRUE,  the mean flow is calculated for each hydrological year separately.
}
 \item{breakdays}{
A vector of break days if the mean flow should be calculated for different seasons.
}
\item{na.rm}{Should missing values be ignored?}
}
\details{
If \code{'breakdays'} is a single day, e.g. "01/06", the start of the hydrological year is taken as the second break day. If more than two seasons are to be specified, a vector of all break days is needed.
}

\value{
A length one vector giving the mean flow for the whole series or the specified
year. If yearly is true, a vector of the annual mean flows is returned. If
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
meanflow(ngaruroro)
meanflow(ngaruroro, breakdays = c("01/11","01/05"))
meanflow(ngaruroro, year = 1991)}
