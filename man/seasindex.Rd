\name{seasindex}
\alias{seasindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasonality Index
}
\description{
Calculates the seasonality index.
}
\usage{
seasindex(lfobj,
          Q = 95,
          na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"}
  \item{Q}{Which quantile to use (standard = Q95)}
  \item{na.rm}{Should missing values be ignored?}
 }

\value{
  {A list descriping the arrow}
  \item{theta}{Angle in radians}
  \item{D}{Julian Date}
  \item{r}{Length}
}

\references{
Laaha, G. and Bl{\"o}schl, G. (2006), Seasonality indices for regionalizing
low flows. Hydrol. Process., 20

Laaha,G. Process Based Regionalisation of Low Flows, Band 198 von Wiener
Mitteilungen, Inst. f{\"u}r Wasserbau u. Ingenieurhydrologie,
Techn. Univ. Wien, 2006, ISBN 3852340896}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{seasindex}}}

\examples{
data(ngaruroro)
#Start of the hydrological year (01/12) is taken as second breakday
seasindex(ngaruroro)
}

