\name{lfnacheck}
\alias{lfnacheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Low flow object check for missing values.
}
\description{
Looks for NAs in a lfobj.
}
\usage{
lfnacheck(lfobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"
}
}

\value{
A list with the total number of NAs, the percentage, the NAs for every
year and the durations of NA-series.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{bfplot}}}

\examples{
data(ngaruroro)
lfnacheck(ngaruroro)
}

