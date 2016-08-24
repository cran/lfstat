\name{lfnainterpolate}
\alias{lfnainterpolate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate missing values
}
\description{
If a lfobj contains missing values, the missing values are replaced by
connecting the last available value before the break and the first after
the break by a straight line.
}
\usage{
lfnainterpolate(lfobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"
}
}

\value{
  \item{lfobj}{An object of class "lfobj"} with interpolated missing values
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}

\author{
Daniel Koffler and Gregor Laaha
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
Check carefully in advance if interpolation is a reasonable choice for
filling the hydrograph
}
\seealso{\code{\link{bfplot}}}

\examples{
data(ngaruroro)

#Part of the ngaruroro series with missing data
hydrograph(ngaruroro,startdate = "1/7/1987", enddate = "1/9/1987",amin = FALSE)

ngaruroroint <- lfnainterpolate(ngaruroro)

#The completed hydrograph
hydrograph(ngaruroroint,startdate = "1/7/1987", enddate = "1/9/1987",amin = FALSE)
}

