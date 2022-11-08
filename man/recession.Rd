\name{recession}
\alias{recession}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recession Constant
}
\description{
Does recession analysis using either the \acronym{MRC} (Master recession curve) or
\acronym{IRS} (individual recession segments) method.
}
\usage{
recession(lfobj,
          method = c("MRC", "IRS"),
          seglength,
          threshold,
          peaklevel = 0.95,
          seasonbreakdays = NULL,
          thresbreaks = c("fixed", "monthly","seasonal"),
          thresbreakdays = NULL,
          plotMRC = TRUE,
          trimIRS = 0,
          na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class \code{'lfobj'}}
  \item{method}{ \code{'MRC'} or \code{'IRS'}}
  \item{seglength}{The length of the duration segments - see the \acronym{WNO}-manual
    and use \code{\link{seglenplot}} to choose a good value.}
  \item{threshold}{The threshold level (70 means Q70)}
  \item{peaklevel}{A level between 0 and 1 or a logical vector, see
    details.}
  \item{seasonbreakdays}{A vector of break days. Needed if the recession
    constant should be calculated individually for different seasons, see
  details.}
  \item{thresbreaks}{ \code{'fixed'} uses a fixed threshold level, \code{'monthly'}
    calculates the threshold for every month separately, \code{'seasonal'}
    calculates thresholds for every season defined using
    \code{'thresbreakdays'}.}
  \item{thresbreakdays}{Needed if \code{thresbreaks = 'seasonal'} to define
    the periods for which separate thresholds should be calculated, see
    details}
  \item{plotMRC}{logical, if TRUE and \code{method = 'MRC'} a plot like
    figure 5.4 in the manual is given.}
  \item{trimIRS}{Should a trimmed mean be used for calculating the
    IRS-constant? (0 means no, 0.1 means trim by 10 \%)}
  \item{na.rm}{Should NAs in the series be ignored?}
}

\details{
For recession analysis it is necessary to define flood discharge peaks
in the hydrograph. Argument \code{peaklevel} defines a day to be a
discharge peak, if \eqn{peaklevel * flow > flow[day before]} and
\eqn{peaklevel * flow > flow[day after]}. Use \code{\link{recessionplot}}
to find a good level or hand a logical vector where TRUE means rain peak.

If  \code{'thresbreakdays'} or  \code{'seasonbreakdays'} is a single day, e.g. \code{'01/06'}, the start of the hydrological year is taken as the second break day. If more than two seasons are to be specified, a vector of all break days is needed.
}

\value{
The overall recession rate in days. If seasons are defined a rate for
every season is calculated.
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{seglenplot}}, \code{\link{recessionplot}}}


\examples{
data(ngaruroro)
recession(ngaruroro,method = "MRC",seglen = 7,threshold = 70)
}

