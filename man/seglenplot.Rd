\name{seglenplot}
\alias{seglenplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Barchart of recession length
}
\description{
Plots a barchart to find a good value for 'seglength' when using
\code{\link{recession}}.
}
\usage{
seglenplot(lfobj,
           threslevel = 70,
           thresbreaks = c("fixed","monthly","seasonal"),
           thresbreakdays = NULL,
           rainpeaklevel = 0.95,
           na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"}
  \item{threslevel}{The threshold level (70 means Q70)}
  \item{thresbreaks}{"fixed" uses a fixed threshold level, "monthly"
    calculates the threshold for every month separatly, "seasonal"
    calculates thresholds for every season defined using
    "thresbreakdays".}
  \item{thresbreakdays}{Needed if "thresbreaks = 'seasonal'" to define
    the periodes for which separate thresholds should be calculated, see
    details}
  \item{rainpeaklevel}{A level between 0 and 1 or a logical vector, see
    details.}
  \item{na.rm}{Should NAs in the series be ignored?}
}

\details{
For recession analysis it is necessary to define flood discharge peaks
(rainpeaks) in the hydrograph. Rainpeaklevel defines a day to be a
discharge peak, if rainpeaklevel * flow > flow[day before] and
rainpeaklevel * flow > flow[day after].
  
If thresbreakdays or seasonbreakdays is a single day, e.g. "01/06", the start of the hydrological year is taken as the second breakday. If more than two seasons are to be specified, a vector of all breakdays is needed.
}

\section{Warning}{Other then in the manual, we implemented a barchart
  instead of a histogram. To save space, empty bars are not plotted!
}
\value{
A barchart
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{recession}}}

\examples{
\dontrun{
data(ngaruroro)
seglenplot(ngaruroro)}
}

