\name{streamdef}
\alias{streamdef}
\title{
Streamflow Deficit
}
\description{
Calculates the streamflow deficit. Deprecated, use \code{\link{find_droughts}} instead.
}
\usage{
streamdef(lfobj,
          pooling = c("none", "MA", "IT", "IC"),
          threslevel = 70,
          thresbreaks = c("fixed","monthly","daily","seasonal"),
          breakdays = c("01/06","01/10"),
          MAdays = 7,
          tmin = 5,
          IClevel = 0.1,
          mindur = 0,
          minvol = 0,
          table = c("all", "volmax", "durmax"),
          na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
An object of class \code{"lfobj"}
}
  \item{pooling}{
The pooling procedure used, "MA" stands for moving average, "IT" is the inter event time and "IC" is Lena Tallaksen's inter event time and volume criterion.
}
  \item{threslevel}{
The threshold level, 70 means that Q70 should be used as threshold
}
  \item{thresbreaks}{
The periods for which separated thresholds should be used, \code{'fixed'} uses a constant threshold, \code{'monthly'} uses monthly breaks, \code{'daily'} takes daily threshold levels. If \code{'seasonal'} is specified, you can enter the break days manually using \code{'breakdays'}.
}
  \item{breakdays}{
A vector of break days if \code{thresbreaks = "seasonal"}. Please enter the break days using the format "%d/%m", e.g. c("01/03", "01/09") uses the first of March an the first of September as breakdays.
}

  \item{MAdays}{
If pooling = "MA" this is the number of days that should be averaged
}
  \item{tmin}{
Defines the number of days that low flow events must be separated within the "IT" or "IC" method.
}
  \item{IClevel}{
The ratio between inter-event excess volume in the "IC" method
}
  \item{mindur}{
 The minimal duration of a low flow event in "IC" and "IT" method
}
  \item{minvol}{
The minimal deficit in a low flow period in "IC" and "IT" method
}
\item{table}{Should the output be a table of \code{"all"} deficit, \code{"volmax"}
  annual volume maxima or \code{"durmax"} annual duration maxima}
\item{na.rm}{Should NAs be removed?}
}
\details{
When method \code{'MA'} is applied, the first and last \code{MAdays/2} are not averaged, their original value is taken instead!
}
\value{
A data frame containing characteristics of all low flow periods.
\item{d}{The duration of the low flow event}
\item{v}{The drought volume (negative Values, as it is a deficit)}
\item{mi}{The drought magnitude, i.e. the (positive) ratio between deficit volume and deficit duration}
\item{Qmin}{The minimum flow of the low flow period}
\item{startyear}{Year of the start of the low flow period}
\item{startmonth}{Month of the start of the low flow period}
\item{startday}{Day of the start of the low flow period}
Please note that when using the "IT" method the end date of the low flow period is not necessarily start date + duration.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{
\code{\link{streamdefplot}}, \code{\link{createlfobj}}, \code{\link{find_droughts}}
}
\examples{
data(ngaruroro)
ng <- subset(ngaruroro, hyear > 1980)

# Full Table
streamdef(ng, pooling = "MA", MAdays = 6)

# Annual Volume-Maxima only
streamdef(ng, pooling = "MA", MAdays = 6,table = "volmax")
}
