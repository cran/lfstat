\name{plot.deficit}
\alias{plot.deficit}
\title{Plot time series of deficits}
\description{
Plot method for objects of class deficit.
}
\usage{
\method{plot}{deficit}(x, type = "dygraph", ...)
}
\arguments{
  \item{x}{object of class deficit}
  \item{type}{if \code{type = "dygraph"} interactive time series plotting is done using the dygraphs JavaScript library. Otherwise \code{\link{plot.xts}} is called. }
  \item{\dots}{further arguments, passed on to the subsequent plot function, e.g. \code{step = FALSE}.}
}


\seealso{
\code{\link{dygraph}}
}
\examples{
data(ray)
r <- find_droughts(ray, threshold = 0.02)
plot(r)

plot(r, step = FALSE)
}

\keyword{low-flow}
\keyword{iplot}
