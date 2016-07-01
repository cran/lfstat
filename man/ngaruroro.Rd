\name{ngaruroro}
\docType{data}
\alias{ngaruroro}
\alias{ray}
\title{Daily stream flow data used for low flow analysis}
\description{
    This data set provides the streamflow records for the rivers Ngaruroro (New Zealand) and Ray (UK). They are provided as a low flow object (lfobj) as used in the package lfstat.}
\usage{data(ngaruroro)

data(ray)}
\format{A lfobj \code{\link{createlfobj}}}
\source{Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.}
\examples{
data(ngaruroro)
hyear_start(ngaruroro)
plot(ngaruroro)

data(ray)
hyear_start(ray)
attr(ray, "lfobj")
}

\keyword{datasets}
