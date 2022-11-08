\name{hydrograph}
\alias{hydrograph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hydrograph
}
\description{
Plots the hydrograph for a given period.
}
\usage{
hydrograph(lfobj, startdate = NULL, enddate = NULL, amin = FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class \code{"lfobj"}}
  \item{startdate}{Begin of hydrograph, date or hydrological year}
  \item{enddate}{End of hydrograph, date or hydrological year}
  \item{amin}{logical, mark annual minima?}
  \item{...}{Additional arguments handed to "plot" - please note that
    some changes e.g. tickmarks on x-axis are not possible}
}

\details{
Start date and end date can be \code{NULL} (first/last date in a low flow object
), a date in
format \code{"dd/mm/yyyy"} (e.g. "01/10/1971") or a year \code{"yyyy"} (e.g 1961).
}

\value{
Plot of hydrograph
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{bfplot}}}


\examples{
data(ngaruroro)
# Full period
hydrograph(ngaruroro)

# Hydrological year 1981 and 1982 with annual minima
hydrograph(ngaruroro, startdate = 1981, enddate = 1982, amin = TRUE)

# From 01/01/1981 to 31/03/1981
hydrograph(ngaruroro, startdate = "01/01/1981", enddate = "31/03/1981")

# Log - yaxis
hydrograph(ngaruroro, startdate = "01/01/1981", enddate = "31/03/1981",log = "y")
}

