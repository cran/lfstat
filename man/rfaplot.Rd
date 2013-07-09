\name{rfaplot}
\alias{rfaplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regional Frequency Analysis
}
\description{
This function uses J.R.M. Hosking's package lmom to produce a L-moment diagram.
}
\usage{
rfaplot(lflist, n = 7,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lflist}{A list of "lfobj"s}
  \item{n}{MAM-n is used (e.g. n=7 means MAM7)}
  \item{...}{is passed to Hosking's function \code{\link{lmrd}}.}
}


\references{
Manual on Low-flow Estimation and Prediction, Operational Hydrology
Report No. 50, Koblenz 2009

J. R. M. Hosking (2012). L-moments. R package, version 1.6. URL:
  http://CRAN.R-project.org/package=lmom.
}
\author{
Daniel Koffler and Gregor Laaha
}

\examples{
data(ngaruroro)

#Toy example to get some more "rivers"
seventies <- subset(ngaruroro, hyear \%in\% 1970:1979)
eighties <- subset(ngaruroro, hyear \%in\% 1980:1989)
nineties <- subset(ngaruroro, hyear \%in\% 1990:1999)

rfaplot(list(seventies,eighties,nineties), n=3)
}
\keyword{Regional Frequency Analysis}
\keyword{lmom}

\seealso{\code{\link{lmom}}, \code{\link{rfa}}}