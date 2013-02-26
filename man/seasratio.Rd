\name{seasratio}
\alias{seasratio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasonality Ratio
}
\description{
Calculates the seasonality ratio for two seasons.
}
\usage{
seasratio(lfobj,
          breakdays,
          Q = 95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"}
  \item{breakdays}{One or two dates defining the summer/winter season}
  \item{Q}{Which quantile to use (standard = Q95)}
 }

\details{
If breakdays is a single day, e.g. "01/06", the start of the hydrological year is taken as the second breakday. If other seasons are to be specified, a vector of two breakdays is needed.
}

\value{
The seasonality ratio.
}

\references{
Laaha, G. and Bl{\"o}schl, G. (2006), Seasonality indices for regionalizing low flows. Hydrol. Process., 20
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{seasindex}}}

\examples{
data(ngaruroro)
#Start of the hydrological year (01/12) is taken as second breakday
seasratio(ngaruroro, breakdays = "01/07")

#Two breakdays
seasratio(ngaruroro, breakdays = c("01/03","01/09"))
}

