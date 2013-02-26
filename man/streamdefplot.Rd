\name{streamdefplot}
\alias{streamdefplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Streamflow Deficite Plot
}
\description{
Gives a plot for a given hydrological year that showes deficite
duration, occurance and volume.
}
\usage{
streamdefplot(lfobj, year, threslevel = 70, thresbreaks = c("fixed",
                 "monthly", "daily", "seasonal"), breakdays =
                 c("01/06", "01/10"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
An object of class "lfobj"
}
  \item{year}{
The hydrological year that should be plotted
}
  \item{threslevel}{
The threshold level, 70 means that Q70 should be used as threshold
}
  \item{thresbreaks}{
The periods for which seperated thresholds should be used, "fixed" uses a constant threshold, "monthly" uses monthly breaks, "daily" takes daily thresholdlevels. If "seasonal" is specified, you can enter the breakdays manualy using "breakdays".
}
  \item{breakdays}{
A vector of breakdays if thresbreaks = "seasonal". Please enter the breakdays using the format "%d/%m", e.g. c("01/03", "01/09") uses the first of March an the first of September as breakdays.
}
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
streamdef
}
\examples{
data(ngaruroro)
streamdefplot(ngaruroro, year = 1991)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{streamflow deficite}
\keyword{low flow}% __ONLY ONE__ keyword per line
