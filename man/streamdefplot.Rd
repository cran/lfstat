\name{streamdefplot}
\alias{streamdefplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Streamflow Deficit Plot
}
\description{
Gives a plot for a given hydrological year that shows deficit
duration, occurrence and volume.
}
\usage{
streamdefplot(lfobj, year, threslevel = 70, thresbreaks = c("fixed",
                 "monthly", "daily", "seasonal"), breakdays =
                 c("01/06", "01/10"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
An object of class \code{'lfobj'}
}
  \item{year}{
The hydrological year that should be plotted
}
  \item{threslevel}{
The threshold level, 70 means that Q70 should be used as threshold
}
  \item{thresbreaks}{
The periods for which separated thresholds should be used, \code{'fixed'} uses a constant threshold, \code{'monthly'} uses monthly breaks, \code{'daily'} takes daily threshold levels. If \code{'seasonal'} is specified, you can enter the break days manually using \code{'breakdays'}.
}
  \item{breakdays}{
A vector of break days if \code{thresbreaks = 'seasonal'}. Please enter the break days using the format \code{'\%d/\%m'}, e.g. c(\code{'01/03'}, \code{'01/09'}) uses the first of March an the first of September as break days.
}
}

\value{No return value, called for side effects (plotting).}


\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}

\seealso{\code{streamdef}}

\examples{
data(ngaruroro)
streamdefplot(ngaruroro, year = 1991)
}

\keyword{streamflow deficit}
\keyword{low flow}
