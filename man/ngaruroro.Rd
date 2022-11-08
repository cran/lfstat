\name{ngaruroro}
\docType{data}
\alias{ngaruroro}
\alias{ng}
\alias{ray}
\title{Daily stream flow data used for low flow analysis}
\description{
    This data set provides the streamflow records for the rivers Ngaruroro (New Zealand) and Ray (UK). They are provided as a low flow object (class \code{'lfobj'}) as used in the package lfstat. The user might want to perform analysis with shorter time series. The data set \code{ng} just contains the eighties (hydrological year 1980 -- 1989) of the Ngaruroro discharges.}
\usage{
data(ngaruroro)
data(ng)
data(ray)}
\format{A low flow object, \code{\link{createlfobj}}}
\source{Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.}
\examples{
data(ngaruroro)
hyear_start(ngaruroro)
plot(ngaruroro)

data(ray)
hyear_start(ray)
attr(ray, "lfobj")
}

\keyword{datasets}
