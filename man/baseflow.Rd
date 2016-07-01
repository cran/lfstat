\name{baseflow}
\alias{baseflow}
\title{Calculate the baseflow of a river}
\description{
Given a streamflow hydrograph of flows (regular time series), the baseflow is separated. The minima of a period (default \code{block.len = 5)} is calculated and turning points are identified. At turning points the baseflow equals the actual flow, in between, linear interpolation is carried out. }
\usage{
baseflow(x, tp.factor = 0.9, block.len = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector containing flows}
  \item{tp.factor}{numeric vector of length one. Towards high flows, allow the
  central value of three consecutive minima only to be of a factor
  \code{(1 - tp.factor)} higher than the surrounding values}
  \item{block.len}{numeric vector of length one. }
}
\value{A numeric vector of \code{length(x)}. It contains \code{NA}s as until the
first turning point, the baseflow cannot be determined.}
\references{
Tallaksen, L. M. and  Van Lanen, H. A. J. 2004 Hydrological Drought: Processes and Estimation Methods for Streamflow and Groundwater. \emph{Developments in Water Science} \bold{48}, Amsterdam: Elsevier.
}
\examples{
## reproducing Tallaksen and van Lanen (2004)
## Example 5.3 Base Flow Index"

data(ray)
ray <- as.xts(ray)

# calculate baseflow and plot it
ray$baseflow <- baseflow(ray$discharge)
ray96 <- ray[format(time(ray), "\%Y") == "1996", ]
plot(ray96$discharge, type = "l")
lines(ray96$baseflow, col = 2)

# aggregated base flows for river Ray
# these are mean flow totals per day, not per year as written
# in Tallaksen and van Lanen (2004)
round(colSums(ray96[, c("discharge", "baseflow")]), 2)
}

\keyword{low-flow}
