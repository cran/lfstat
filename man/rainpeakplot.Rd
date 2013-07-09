\name{recessionplot}
\alias{recessionplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recession diagnostic plot
}
\description{
Helps to define peaklevel of a lfobj and visualises recession periodes.
}
\usage{
recessionplot(lfobj,
              peaklevel = 0.95,
              plot = TRUE,
              peakreturn = FALSE,
              thresplot = TRUE,
              threscol = "blue",
              threshold = 70,
              thresbreaks = c("fixed","monthly","seasonal"),
              thresbreakdays = c("01/06","01/10"),
              recessionperiod = TRUE,
              recessioncol = "darkblue",
              seglength = 7,
              ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{A object of class lfobj}
  \item{peaklevel}{A level between 0 and 1 or a logical vector, see
    details.}
  \item{plot}{Should a plot be made}
  \item{peakreturn}{Should a logical with rainpeaks be returned}
  \item{thresplot}{Should the threshold be plotted}
  \item{threscol}{Color of threshold in plot}
  \item{threshold}{Threshold level (70 refers to Q70)}
  \item{thresbreaks}{"fixed" uses a fixed threshold level, "monthly"
    calculates the threshold for every month separatly, "seasonal"
    calculates thresholds for every season defined using
    "thresbreakdays".}
   \item{thresbreakdays}{Needed if "thresbreaks = 'seasonal'" to define
    the periodes for which separate thresholds should be calculated, see
    details}
  \item{recessionperiod}{Should recession periodes be marked}
  \item{recessioncol}{Color of recessionperiod marks}
  \item{seglength}{The minimum number of days to be marked as recession period}
  \item{...}{Further arguments handed to \code{\link{hydrograph}}}.
}

\details{
For recession analysis it is neccessary to define flood discharge peaks
in the hydrograph. Peaklevel defines a day to be a
discharge peak, if peaklevel * flow > flow[day before] and
peaklevel * flow > flow[day after].

This function can be used to check different values of peaklevel.
}

\value{If peakreturn = TRUE: A logical vector giving rainpeaks as TRUE}

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
#To few points identified as peak flood discharge
recessionplot(ngaruroro, peaklevel = .5, start = 1991, end = 1991)

#To many
recessionplot(ngaruroro, peaklevel = .999, start = 1991, end = 1991)

#Good choice?
recessionplot(ngaruroro, peaklevel = .92, start = 1991, end = 1991)

#Getting peakdays for 1991
peak <- recessionplot(ngaruroro, peaklevel = .92, plot = FALSE)
rain1991 <- subset(ngaruroro, subset = hyear == 1991 && peak, select = c(day, month, year))}

}



