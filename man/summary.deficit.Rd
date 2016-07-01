\name{summary.deficit}
\alias{summary.deficit}

\title{Object Summaries}
\description{
Summarizes an object of class deficit. For every drought event the start, end as well as the drought volume and duration is listed.
}
\usage{
\method{summary}{deficit}(object, drop_minor = c(volume = "0.5\%", duration = 5), ...)
}

\arguments{
  \item{object}{an object of class deficit, as produced by \code{\link{find_droughts}}.}
  \item{drop_minor}{a vector of length one or two, determining the filtering of minor droughts. If \code{drop_minor} is of length one and its value is zero, no filtering is applied. Also a numeric of character vector of length two with the named elements \code{volume} and \code{duration} is accepted. If a value contains the percentage (\%) sign this percentage of the maximum duration or volume is used a the filter criterion.}
  \item{\dots}{currently ignored.}
}
\value{
a data.frame where each row corresponds to an event. There are summarizing columns
 \item{event.no}{the event id}
 \item{start}{the starting day of the drought event}
 \item{time}{the day which the event is attributed to. Usually identical with column \code{start}, unless the object \code{x} is the result of the Sequent Peak Algorithm.}
 \item{volume}{the volume of the drought event in cubic meters}
 \item{duration}{the duration of the drought event in days}
 \item{dbt}{days below threshold. Number of days the discharge is lower than the given threshold.}
 \item{qmin}{the minimum discharge}
 \item{tqmin}{date of the minimum discharge}
}

\examples{
data(ray)
r <- find_droughts(ray, threshold = 0.02)
summary(r)      # minor events got filtered

summary(r, drop_minor = 0)      # no filtering
summary(r, drop_minor = c("volume" = 10000, "duration" = 5))
summary(r, drop_minor = c("volume" = "10\%", "duration" = 5))
}

\keyword{low-flow}
\keyword{print}
