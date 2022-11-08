\name{water_year}
\alias{water_year}

\title{Compute the water year}
\description{
Given a date, compute the corresponding water year (hydrological year).
}
\usage{
water_year(x, origin = "din", as.POSIX = FALSE,
           assign = c("majority", "start", "end"), ...)
}

\arguments{
  \item{x}{
a vector, implicit coercion to class \code{'POSIXlt'} is performed.
}
  \item{origin}{
a vector of length one specifying the month in which the hydrological year starts.  Four different ways of defining the beginning of a hydrological year are supported: a character string like \code{'din'} or \code{'usgs'} representing a definition of an institution (see Details), an integer number between 1 and 12, a character string of the month name (possibly abbreviated) or POSIX/Date object from which only the month is taken.
}
  \item{as.POSIX}{logical, if TRUE return value is of class \code{POSIXct}. Otherwise a factor is returned.}
  \item{assign}{
a character vector of length one, deciding how a hydrological year is labelled. Depending on the climate, the hydrological year can start earlier or later than the calendar year. Usually the hydrological year "equals" the calendar year for the longest period of months they have in common. Alternatively a water year can also be designated by the calendar year in which it starts or ends.
}
  \item{\dots}{
arguments, passed on to \code{\link{as.POSIXlt}}, e.g. such as \code{format}
}
}
\details{Currently, it is only supported to start a hydrological year on the 1st of a month.

There are abbreviations for a few established definitions:
  \tabular{lll}{
   \tab \bold{start} \tab \bold{description} \cr
  \code{'din'} \tab 1st of November \tab \acronym{DIN} 4049 (default), as used in Austria and Germany \cr
  \code{'usgs'} \tab 1st of October\tab  \acronym{USGS}, the United States Geological Survey\cr
  \code{'swiss'} \tab 1st of October\tab as defined by the Swiss "Bundesamt f. Energie" (\acronym{BFE}) \cr
  \code{'glacier'} \tab 1st of September \tab Widely used in glaciology \cr
  }

Its convenient to have the water year as a factor with levels even for year without observations. For example, otherwise years without observations don't appear after aggregation.
}
\value{
a factor representing the hydrological year. }
%\references{}
%\note{}


\examples{
# generating monthly sequence
x <- seq(from = as.Date("1992-01-01"),
         by = "months", length.out = 12)

# specifying the beginning with a decimal number
water_year(x, origin = 10)

# using a month name
water_year(x, origin = "Jul")      # can be abbreviated
water_year(x, origin = "july")     # case insensitive

# using an POSIX or Date object
water_year(x, origin = as.Date("2012-08-22"))     # only month is taken
water_year(x, origin = as.POSIXct("2012-08-22"))

# or by specifying an institution
water_year(x, origin = "usgs")
}

\keyword{low-flow}
