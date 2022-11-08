\name{find_droughts}
\alias{find_droughts}

\title{Identifying Low Flow Periods}
\description{
A streamflow deficit is defined as an event where discharges are below a given threshold.
}
\usage{
find_droughts(x, threshold = vary_threshold, varying = "constant", interval = "day", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object which can be coerced to class \code{'xts'}. Either with a single column or with a column named \code{'discharge'}. Units of \code{'x'} are retrieved from the attributes. If it fails, an error is raised.
}
  \item{threshold}{The threshold can either be a constant value, a time series with the same length as \code{x} or a function (taking a single argument) returning either of these. Furthermore \code{threshold} can be a character vector of length one specifying a quantile of x like \code{threshold = 'Q80'} as a shortcut of \code{threshold = function(x) quantile(x, 0.2, na.rm = TRUE)}. See \code{\link{vary_threshold}} for varying a threshold in time.}
  \item{varying}{if \code{varying} is a character vector of length one, values of "constant", "daily", "weekly" and "monthly" are allowed. If a vector of class POSIX is provided, a seasonal varying threshold is computed, where the times provided define the start of the season. Only the day of the year is taken, the year itself doesn't have a meaning.}
  \item{interval}{A character string, containing one of "day", "week", "month", "quarter" or "year" as accepted by \code{\link{seq.Date}}.}
  \item{\dots}{
if threshold is a function, these additional arguments are passed on to the function
}
}

\value{
an object of class \code{'deficit'}, which is basically an \code{'xts'} object with the columns
\item{discharge}{discharges as provided with \code{x}}
\item{threshold}{the threshold}
\item{def.increase}{The increase of the deficit volume in m^3 per day. }
\item{event.no}{an event id. If an event is numbered "0" this period not considered as a streamflow deficit.}
}


\seealso{
There are summary and plot methods, see \code{\link{summary.deficit}} and \code{\link{plot.deficit}}.

\code{\link{pooling}}, \code{\link{summary.deficit}}, \code{\link{plot.deficit}}
}
\examples{
data(ray)
ray <- as.xts(ray)["1970::1979", ]
r <- find_droughts(ray)
head(r)
summary(r)

\donttest{
plot(r)
}

# threshold is to low, because there are many days with
# zero flow observations
# provide threshold as a constant value
r <- find_droughts(ray, threshold = 0.02)
head(r)
summary(r)
\donttest{
plot(r)
}


# provide threshold as a function
r <- find_droughts(ray,
                   threshold = function(x) quantile(x, 0.2, na.rm = TRUE))
head(r)
summary(r)
}

\keyword{low-flow}
