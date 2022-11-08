\name{as.lfobj}
\alias{as.lfobj}
\alias{as.lfobj.xts}
\alias{as.lfobj.zoo}
\alias{is.lfobj}

\title{
Coerce to class \code{'lfobj'}
}
\description{
Functions to check if object is of class \code{'fobj'} or coerce it if possible. Currently, only methods for \code{'zoo'} and \code{'xts'} exist.
}
\usage{
as.lfobj(x, ...)
is.lfobj(x)
\method{as.lfobj}{xts}(x, ...)
\method{as.lfobj}{zoo}(x, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{any \R object.}
  \item{\dots}{additional arguments to be passed to or from methods.}
}

\value{
An object of class \code{'lfobj'}.
}



\seealso{
\code{\link{createlfobj}}
}
\examples{

data(ngaruroro)
is.lfobj(ngaruroro)

# coerce zoo object to class \code{'lfobj'}
z1 <- zoo(1:10, order.by = seq(Sys.Date(), length.out = 10, by = "days"))
as.lfobj(z1, hyearstart = 5)

# coerce xts object to class \code{'lfobj'}
xts1 <- xts(1:10, order.by = seq(Sys.Date(), length.out = 10, by = "days"))
as.lfobj(xts1, hyearstart = 5)



}


\keyword{ methods }
\keyword{ manip }
