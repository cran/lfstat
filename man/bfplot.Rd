\name{bfplot}
\alias{bfplot}

\title{
Base Flow Plot
}
\description{
Visualizes the hydrograph versus the base flow hydrograph.
}
\usage{
bfplot(lfobj,
       year = "any",
       col = "green",
       bfcol = "blue",
       ylog = FALSE)
}

\arguments{
  \item{lfobj}{
An object of class \code{"lfobj"}
}
  \item{year}{
The hydrological year for which the BFI should be computed. If "any" the
whole series is plotted.
}
\item{col}{Colour of flow}
\item{bfcol}{Colour of base flow}
\item{ylog}{Log y-axis?}
}

\value{No return value, called for side effects (plotting).}

\references{Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{
\code{\link{BFI}}
}
\examples{
data(ngaruroro)
# Plot starts in December, as ngaruroro's hyearstart = 12
bfplot(ngaruroro, year = 1991)
}
\keyword{base flow}

