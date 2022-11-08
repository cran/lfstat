\name{seglenplot}
\alias{seglenplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bar chart of recession length
}
\description{
Plots a bar chart to find a good value for argument \code{'seglength'} when using
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
  \item{lfobj}{An object of class \code{'lfobj'}}
  \item{threslevel}{The threshold level (70 means Q70)}
  \item{thresbreaks}{\code{'fixed'} uses a fixed threshold level, \code{'monthly'}
    calculates the threshold for every month separately, \code{'seasonal'}
    calculates thresholds for every season defined using
    \code{'thresbreakdays'}.}
  \item{thresbreakdays}{Needed if \code{thresbreaks = 'seasonal'} to define
    the periods for which separate thresholds should be calculated, see
    details}
  \item{rainpeaklevel}{A level between 0 and 1 or a logical vector, see
    details.}
  \item{na.rm}{Should NAs in the series be ignored?}
}

\details{
For recession analysis it is necessary to define flood discharge peaks
(rain peaks) in the hydrograph. Argument \code{rainpeaklevel} defines a day to be a
discharge peak, if \code{rainpeaklevel * flow > flow[day before]} and
\code{rainpeaklevel * flow > flow[day after]}.

If  \code{'thresbreakdays'} or  \code{'seasonbreakdays'} is a single day, e.g. \code{'01/06'}, the start of the hydrological year is taken as the second break day. If more than two seasons are to be specified, a vector of all break days is needed.
}

\section{Warning}{Other then in the manual, we implemented a bar chart
  instead of a histogram. To save space, empty bars are not plotted!
}
\value{
A bar chart
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\seealso{\code{\link{recession}}}

\examples{
data(ngaruroro)
seglenplot(ngaruroro)
}
