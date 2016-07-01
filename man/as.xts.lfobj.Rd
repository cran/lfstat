\name{as.xts.lfobj}
\alias{as.xts.lfobj}

\title{
Convert Object To Class xts
}
\description{
Conversion function to coerce data objects of classes lfobj to class xts.}
\usage{
\method{as.xts}{lfobj}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class lfobj.
}
  \item{\dots}{
additional parameters or attributes
}
}
\value{An S3 object of class xts.}

\seealso{
\code{\link{as.xts}}
}

\examples{
data(ray)
r <- as.xts(ray)

# attributes of the lfobject are retained
attr(ray, "lfobj")
xtsAttributes(r)
}

\keyword{low-flow}
