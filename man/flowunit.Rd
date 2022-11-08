\name{flowunit}
\alias{flowunit}
\alias{flowunit.lfobj}
\alias{flowunit.xts}
\alias{flowunit<-}
\alias{flowunit<-.lfobj}
\alias{flowunit<-.xts}

\title{Set and retrieve unit of the discharge}
\description{
In order to compute deficit volumes time series of discharges (either of class \code{'lfobj'} or \code{'xts'}) \code{\link{summary.deficit}} needs to be aware of the unit. Units are stored in the attributes of the time series. \code{flowunit(x)} retrieves the current unit from the attributes, \code{flowunit(x) <- value} sets a new one.
}
\usage{
flowunit(x)
\method{flowunit}{xts}(x)
\method{flowunit}{lfobj}(x)

flowunit(x) <- value
\method{flowunit}{xts}(x) <- value
\method{flowunit}{lfobj}(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The time series, either of class \code{'lfobj'} or \code{'xts'}.}
  \item{value}{ a valid character string of length one that can be interpreted as flow unit. See details.}
}

\details{Currently, just a few functions like \code{\link{summary.deficit}} and \code{lfstat:::plot.deficit_dygraph} make use of the unit stored as an attribute.

Usually flow units are of dimension $L^3 T^{-1}$. Currently a length $l$ can be on of \code{c("metre", "cm", "centimetre" "litre")}, whereas time $T$ can be one in \code{c("days", "hours", "mins", "secs")}, possibly abbreviated. The numerator of the fraction (everything before the literal \code{"/"}) is interpreted as the length (superscripts like \code{"^3"} are discarded), the denominator as time. E.g. valid units would be \code{"cm^3/s"}, \code{"m^3/day"} or \code{"litre/sec"}.
}

\value{
A character vector of length one, containing the currently used discharge unit.
}
\examples{
data(ray)
ray <- as.xts(ray)["1970::1970", ]

# currently discharges are in cubic metres per second
flowunit(ray)

# calculating deficit volumes, for fixed threshold 0.001 m^3/s
(s <- summary(find_droughts(ray, threshold = 0.001)))

# multiplying the discharge by 1000 converts is to litre per second
ray$discharge <- ray$discharge * 1000

# changing the unit accordingly, yields the same volumes
flowunit(ray) <- "l/s"
(ss <- summary(find_droughts(ray, threshold = 1)))

identical(s$volume, ss$volume)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ attribute }
\keyword{ utilities }
