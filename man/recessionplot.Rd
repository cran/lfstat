\name{recessionplot}
\alias{recessionplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recession diagnostic plot
}
\description{
Helps to define the peak level of a low flow object and visualises recession periods.
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
  \item{lfobj}{A object of class \code{'lfobj'}}
  \item{peaklevel}{A level between 0 and 1 or a logical vector, see
    details.}
  \item{plot}{Should a plot be made}
  \item{peakreturn}{Should a logical with rain peaks be returned}
  \item{thresplot}{Should the threshold be plotted}
  \item{threscol}{Colour of threshold in plot}
  \item{threshold}{Threshold level (70 refers to Q70)}
  \item{thresbreaks}{"fixed" uses a fixed threshold level, "monthly"
    calculates the threshold for every month separately, "seasonal"
    calculates thresholds for every season defined using
    \code{'thresbreakdays'}.}
   \item{thresbreakdays}{Needed if \code{thresbreaks = 'seasonal'} to define
    the periods for which separate thresholds should be calculated, see
    details}
  \item{recessionperiod}{Should recession periods be marked}
  \item{recessioncol}{Colour of recession period marks}
  \item{seglength}{The minimum number of days to be marked as recession period}
  \item{...}{Further arguments handed to \code{\link{hydrograph}}}.
}

\details{
For recession analysis it is necessary to define flood discharge peaks
in the hydrograph. The peak level defines a day to be a
discharge peak, if \code{peaklevel * flow > flow[day before]} and
\code{peaklevel * flow > flow[day after]}.

This function can be used to check different values of the peak level.
}

\value{If \code{'peakreturn = TRUE'}: A logical vector giving rain peaks as \code{TRUE}}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{recession}}}
\examples{
data(ngaruroro)
# To few points identified as peak flood discharge
recessionplot(ngaruroro, peaklevel = .5, start = 1991, end = 1991)

# To many
recessionplot(ngaruroro, peaklevel = .999, start = 1991, end = 1991)

# Good choice?
recessionplot(ngaruroro, peaklevel = .92, start = 1991, end = 1991)

# Getting peakdays for 1991
peak <- recessionplot(ngaruroro, peaklevel = .92, plot = FALSE, peakreturn = TRUE)
rain1991 <- subset(ngaruroro, subset = (hyear == 1991) & peak, select = c(day, month, year))
}



