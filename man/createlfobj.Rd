\name{createlfobj}
\alias{createlfobj}
\alias{createlfobj.default}
\alias{createlfobj.data.frame}
\alias{createlfobj.ts}
\alias{createlfobj.lfobj}

\title{
Create an lfobj for further Low Flow Analysis
}
\description{
Generic function for creating a low flow object (lfobj). Low flow objects can be
created from a time series of daily flow, a data.frame with columns
"flow", "day", "month" and "year".
}
\usage{
createlfobj(x, ...)


\method{createlfobj}{data.frame}(x, hyearstart = NULL, baseflow = TRUE,
            meta = list(),...)

\method{createlfobj}{ts}(x,
            startdate,
            dateformat = "\%d/\%m/\%Y",
            ...)

\method{createlfobj}{lfobj}(x, hyearstart = NULL, baseflow = NULL,
            meta = NULL,...)
}
\arguments{
  \item{x}{
An object out of which a lfobj should be created
}
  \item{hyearstart}{
integer between 1 and 12, indicating the start of the hydrological year.
}
\item{baseflow}{
  logical, should the baseflow curve be calculated? Needed, if you want
  to apply 'bfplot' or 'BFI' later on.
}
\item{meta}{A list of meta-information}
\item{startdate}{start of the time-series}
\item{dateformat}{Format of the startdate}
\item{...}{Additional arguments, passed on to \code{createlfobj.data.frame}.}
}
\details{
'hyearstart' defines the starting month of the hydrological year. If
'hyearstart' is greater then 6.5, the hydrological year starts earlier
then the actual date, e.g. hyearstart = 10, then the 1st of October 2011
is part of the hydrological year 2012. If hyearstart = 4, then the 31st
of March 2011 is part of the hydrological year 2010.

When creating an object of class \code{lfobj} with the aforementioned functions, eventually \code{createlfobj.data.frame} is called.
}

\value{
An object of class 'lfobj'.
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}

\seealso{
\code{\link{readlfdata}}
}
\examples{
#Creating a lfobj from a timeseries
#Some sample data:

somevalues <- rexp(365)
#Convert to time series:
time <- ts(somevalues)

#Lets say our data contains values from one hydrological year (Oct-Sep)
#starting on 1. Oct. 1992:
myriver <- createlfobj(time, startdate = "01/10/1992",hyearstart = 10)
#Add meta-data
createlfobj(myriver, meta = list(river = "myriver"))
}

\keyword{ lfobj }

