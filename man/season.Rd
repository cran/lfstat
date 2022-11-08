\name{season}
\alias{season}

\title{
Attribute dates to seasons
}
\description{
Based on a vector of breaks (start dates) dates are classified into seasons.
}
\usage{
season(x, start = c(winter = as.Date("2005-12-01"),
                    spring = as.Date("2005-03-01"),
                    summer = as.Date("2005-06-01"),
                    autumn = as.Date("2005-09-01")))
}


\arguments{
  \item{x}{
 Vector of dates to be classified into seasons. Methods for class \code{'numeric'}, \code{'Date'} and \code{'POSIXct'} exist. If input is numeric it is assumed to be the day of the year (see \code{\link{strptime}} \code{'\%j'}).
}
  \item{start}{Possibly named vector of starts of a season. If the vector is
  unnamed generic names are used and a warning is risen.
}
}

\value{
Factor of classifications of seasons.
}

\seealso{
\code{link{apply.seasonal}}
}
\examples{
# input vector is of class Date
times <- seq(from = Sys.Date(), to = Sys.Date() + 500, by = 20)
season(times)

# input vector is numeric (the day of the year)
n <- as.numeric(format(times, "\%j"))
season(n)

identical(season(times), season(n))
}

\keyword{ chron }
