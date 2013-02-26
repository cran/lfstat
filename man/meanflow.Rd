\name{meanflow}
\alias{meanflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean flow
}
\description{
Calculates the meanflow of an object of class 'lfobj'.
}
\usage{
meanflow(lfobj, year = "any", monthly = FALSE, yearly = FALSE,
breakdays = NULL, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"
}
  \item{year}{
The year for which the meanflow should be computed. If hyearstart != 1 the meanflow is calculated for the hydrological year! "any" means the whole series should be taken.
}
  \item{monthly}{
logical - Should the meanflow be calculated separately for every month?. 
}
  \item{yearly}{
logical - If TRUE,  the meanflow is calculated for each hydrological year separately. 
} 
 \item{breakdays}{
A vector of breakdays if the meanflow should be calculated for different seasons. 
}
\item{na.rm}{Should missing values be ignored?}
}
\details{
If breakdays is a single day, e.g. "01/06", the start of the hydrological year is taken as the second breakday. If more than two seasons are to be specified, a vector of all breakdays is needed.
}

\value{
A length one vector giving the meanflow for the whole series or the specified
year. If yearly is true, a vector of the annual meanflows is returned. If
breakdays are specified, the values are separated per season.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{MAM}}}

\examples{
data(ngaruroro)
meanflow(ngaruroro)
meanflow(ngaruroro, breakdays = c("01/11","01/05"))
meanflow(ngaruroro, year = 1991)}
