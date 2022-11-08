\name{lfstat-package}
\alias{lfstat-package}
\alias{lfstat}
\docType{package}
\title{
\packageTitle{lfstat}
}
\description{
\packageDescription{lfstat}
}
\details{

\subsection{Create an object of class \code{lfobj} (Low-Flow-Objects)}{
The package calculates indices and makes graphics for low flow
analysis. It brings its own class \code{'lfobj'}, a special data.frame format
with columns \code{'day'}, \code{'month'}, \code{'year'}, \code{'flow'}, \code{'hyear'} and possibly \code{'baseflow'}.

\code{'day'}, \code{'month'} and \code{'year'} refer to the date, \code{'flow'} is the measured
runoff (unit-independent), \code{'baseflow'} the calculated base flow.

\code{'hyear'} refers to the hydrological year. When creating the \code{'lfobj'} you
define the month where the stations hydrological year starts. If annual indices are
calculated or single years are plotted, the \code{'hyear'} is taken.

Basically there are to options to create an low flow object:

If you have special data format, e.g. \acronym{GRDC}, you can use the function
\code{\link{readlfdata}}, see \code{?readlfdata} to see which formats are currently
supported.
Otherwise you can use \code{\link{createlfobj}}. You can apply it for new data in
one of two ways:
1) You create a data.frame with columns: \code{'day'}, \code{'month'}, \code{'year'} and
\code{'flow'}.
2) You create a time-series (\code{'ts'}) from \code{'flow'} and give the start date of
the series when calling \code{'createlfobj'}.
}


\subsection{Preparation}{
lfstat does not need to know the unit of the flow, but you might want it
to appear in your plots. You can use \code{\link{setlfunit}} to define how units are
labelled in your graphics. Examples are given in \code{'?setlfunit'}.

Please check for NA-values using \code{\link{lfnacheck}}, indices and plots are made
as if series were complete. See the manual on how to deal with missing
values and, if reasonable, use \code{\link{lfnainterpolate}}.
}

\subsection{Available Indices}{
\itemize{
\item \code{\link{meanflow}}
\item \code{\link{Qxx}}
\item \code{\link{MAM}} (mean annual minima)
\item \code{\link{BFI}}
\item \code{\link{recession}} (recession constant)
\item \code{\link{streamdef}} (Streamflow deficit)
\item \code{\link{tyears}} (Extreme value - T-years event)
\item \code{\link{seasratio}}
\item \code{\link{seasindex}}
\item \code{\link{multistationsreport}}
}
}

\subsection{Plots}{
\itemize{
\item \code{\link{hydrograph}}
\item \code{\link{recessionplot}} (Diagnosis for recession)
\item \code{\link{fdc}} (Flow-duration-curve)
\item \code{\link{sbplot}} (seasonal bar chart)
\item \code{\link{seglenplot}} (select recession length for \code{\link{recession}})
\item \code{\link{streamdefplot}} (Streamflow deficit)
\item \code{\link{rfa}} (Regional frequency analysis)
\item \code{\link{dmcurve}} (Double mass curve)
}
}

\packageIndices{lfstat}
}
\author{
\packageAuthor{lfstat}

Maintainer: \packageMaintainer{lfstat}
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p. \url{https://library.wmo.int/doc_num.php?explnum_id=7699}
}

\keyword{ package }
\keyword{ low flow }
\keyword{ WNO }
