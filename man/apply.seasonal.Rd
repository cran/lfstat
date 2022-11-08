\name{apply.seasonal}
\alias{apply.seasonal}

\title{
Apply an aggregation function seasonally.
}
\description{
Similar to the functions \code{\link{apply.daily}}, \code{\link{apply.monthly}}, \code{\link{apply.yearly}} etc. from the \pkg{xts} package.
}
\usage{
apply.seasonal(x, varying, fun = function(x) min(x, na.rm = TRUE),
               aggregate = NULL, replace.inf = TRUE, origin = 1)
}
\arguments{
  \item{x}{an object of class \code{'xts'} or \code{'zoo'}. The time series which should get aggregated.}
  \item{varying}{a character vector of length one of a possibly named vector of class  \code{'Date'} or coercible to  \code{'Date'}. Valid character vectors are \code{"daily"}, \code{"weekly"}, \code{"monthly"} or \code{"constant"}. If of class  \code{'Date'}, the elements are considered as start points of a season. See Examples.}
  \item{fun}{the function used for aggregating all elements of a season.}
  \item{aggregate}{possibly a function used for aggregating per season.}
  \item{replace.inf}{should non-finite values introduced by \code{fun} be omitted?}
  \item{origin}{The start of the hydrological year. If set to 1 (the default) aggregation is carried out using the calendar year.}
}
\value{a matrix with every (hydrological) year being a row and every column being a season.}

\examples{
 data(ngaruroro)
 ng <- as.xts(ngaruroro)

 year <- water_year(time(ng), origin = "Sept")
 ng10 <- ng[year \%in\% 1991:2000, ]

 # computes the annual minima (AM)
 apply.seasonal(ng10, varying = "yearly", origin = 9)

 # computes the mean annual minima (MAM)
 apply.seasonal(ng10, varying = "yearly", aggregate = mean, origin = 9)

 # computes monthly minima (AM)
 apply.seasonal(ng10, varying = "monthly", origin = 9)

 # computes minima for summer and winter separately
 # winter starts in September
 seasons <- as.Date(c("1999-09-01", "1999-11-04"))
 names(seasons) <- c("winter", "summer")
 apply.seasonal(ng10$discharge, varying = seasons, origin = 9)
}
