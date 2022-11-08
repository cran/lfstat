\name{sbplot}
\alias{sbplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasonal Bar Chart
}
\description{
Plots a seasonal bar chart for daily streamflow data
}
\usage{
sbplot(lfobj, hyearorder = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
A low flow object, as created with \code{createlfobj}
}
\item{hyearorder}{
  logical, if TRUE the bars are plotted according to the hydrological
  year, if FALSE they start with January.
}}

\value{An object of class \code{trellis}, see \code{\link{barchart}}.}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}


\seealso{
\code{\link{createlfobj}}
}
\examples{
data(ngaruroro)
sbplot(ngaruroro)

# Starting with january
sbplot(ngaruroro, hyearorder = FALSE)
}
