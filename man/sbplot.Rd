\name{sbplot}
\alias{sbplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasonal Barchart
}
\description{
Plots a seasonal barchart for daily streamflow data
}
\usage{
sbplot(lfobj,hyearorder = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
A lfobj, as created with 'createlfobj'
}
\item{hyearorder}{
  logical, if TRUE the bars are plotted according to the hydrological
  year, if FALSE they start with January.
}}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
createlfobj
}
\examples{
data(ngaruroro)
sbplot(ngaruroro)
#Starting with january
sbplot(ngaruroro, hyearorder = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
