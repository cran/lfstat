\name{fdc}
\alias{fdc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flow Duration Curve
}
\description{
Plots the flow duration curve for a given low flow object.
}
\usage{
fdc(lfobj, year = "any", breakdays = NULL, colors = TRUE,
    xnorm = FALSE, ylog = TRUE, legend = TRUE, separate = FALSE,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class \code{"lfobj"}
}
  \item{year}{numeric - The year for which the flow duration curve should be computed. If \code{hyearstart != 1} the \acronym{BFI} is calculated for the hydrological year, \code{any = "any"} means the whole series should be taken.
  }
  \item{breakdays}{A vector of break days if the  \acronym{BFI} should be calculated for different seasons.
  }
  \item{colors}{
    logical - If \code{'breakdays'} are specified, should the different flow duration curves are
    displayed in different colours?
  }
  \item{xnorm}{logical - should the x-axis be normalized?}
  \item{ylog}{logical - The the logarithm of the y-axis?}
  \item{legend}{logical - Should a legend be plotted?}
  \item{separate}{logical - Should a separate plot be drawn for every
    season?}
  \item{\dots}{Graphical parameters handed to plot}
}
\details{
If \code{breakdays} is a single day, e.g. "01/06", the start of the hydrological year is taken as the second break day. If more than two seasons are to be specified, a vector of all break days is needed.
}
\value{
A vector of quantiles.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{ecdf}}
}
\examples{
data(ngaruroro)
fdc(ngaruroro,year = 1991)
%fdc(ngaruroro, year = 1991, breakdays = c("01/12","01/06"))
%fdc(ngaruroro, year = 1991, breakdays = "01/06", color = FALSE)
}

\keyword{Flow duration curve }

