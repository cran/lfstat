\name{hyear_start}
\alias{hyear_start}
\alias{hyear_start<-}
\alias{hyear_start<-.xts}
\alias{hyear_start<-.lfobj}
\alias{hyear_start.xts}
\alias{hyear_start.data.frame}

\title{
Extract or guess the Start of a Hydrological Year
}
\description{
Retrieve the start of a hydrological year either from the attributes or from the column \code{'hyear'} of an object of class \code{'lfobj'}.
}
\usage{hyear_start(x, abbreviate = FALSE)

\method{hyear_start}{data.frame}(x, abbreviate = FALSE)

\method{hyear_start}{xts}(x, abbreviate = FALSE)

hyear_start(x) <- value
\method{hyear_start}{xts}(x) <- value
\method{hyear_start}{lfobj}(x) <- value
}


\arguments{
  \item{x}{object of which the start of the hydrological year should be determined.}
  \item{abbreviate}{logical. Should the names be abbreviated?}
  \item{value}{numeric vector of length one. Month in which the hydrological year starts.}

}
\details{
If a valid start of an hydrological year is found in the attributes, it is returned. Otherwise if a column \code{'hyear'} exists, it is used. If this is note possible the integer number one is returned (for January) and a warning is issued.
}
\value{
a vector of length one, either of type character (\code{abbreviate = TRUE}) or numeric.
}

\seealso{
\code{\link{water_year}}
}
\examples{
data(ngaruroro)
hyear_start(ngaruroro)

data(ray)
hyear_start(ray, abbreviate = TRUE)
}

\keyword{low-flow}
\keyword{attribute}
