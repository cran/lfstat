\name{streamdef}
\alias{streamdef}
\title{
Streamflow Deficite
}
\description{
Calculates the streamflow deficite. Deprecated, use \code{\link{find_droughts}} instead.
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
An object of class "lfobj"
}
  \item{pooling}{
The pooling procedure used, "MA" stands for moving average, "IT" is the inter event time and "IC" is Lena Tallaksens interevent time and volume criterion.
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

  \item{MAdays}{
If pooling = "MA" this is the number of days that should be averaged
}
  \item{tmin}{
Defines the number of days that low flow events must be seperatet within the "IT" or "IC" method.
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
\item{table}{Should the output be a table of "all" deficit, "volmax"
  annual volume maxima or "durmax" annual duration maxima}
\item{na.rm}{Should NAs be removed?}
}
\details{
When method "MA" is applied, the first and last MAdays/2 are not averaged, their original value is taken instead!
}
\value{
A data frame containing characteristics of all low flow periods.
\item{d}{The duration of the low flow event}
\item{v}{The drought volume (negative Values, as it is a deficite)}
\item{mi}{The drought magnitude, i.e. the (positive) ratio between deficit volume and deficite duration}
\item{Qmin}{The minimum flow of the low flow period}
\item{startyear}{Year of the start of the low flow period}
\item{startmonth}{Month of the start of the low flow period}
\item{startday}{Day of the start of the low flow period}
Please note that when using the "IT" method the end date of the low flow periode is not necessarily startdate + duration.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{
streamdefplot, createlfobj, \code{\link{find_droughts}}
}
\examples{
data(ngaruroro)
#Full Table
streamdef(ngaruroro,pooling = "MA", MAdays = 6)
#Annual Volume-Maxima only
streamdef(ngaruroro,pooling = "MA", MAdays = 6,table = "volmax")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
