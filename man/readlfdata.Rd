\name{readlfdata}
\alias{readlfdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads data sheets
}
\description{
Reads data sheets of different formats directly as lfobjs.
}
\usage{
readlfdata(file, type = c("GRDC", "HZB", "LFU", "TU"), lfobj = TRUE, readmeta = TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The name of the file which the data are to be read from.}
  \item{type}{The style of the sheet, currently the following formats
are accepted: "GRDC", "HZB" (Austria), "LFU" (Germany, Bavaria), "TU"
(Technical University Vienna)}
  \item{lfobj}{logical, should a lfobj be created?}
  \item{readmeta}{logical, should metainformation from data sheets be
    saved?}
  \item{...}{Handed to createlfobj, could be "hyearstart", "baseflow" or
  "meta", if "readmeta" is FALSE}
}
\value{
A lfobj or data.frame depending on "lfobj".
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\note{
If you like other file formats (national standards)
to be includes, send some examples with a remark how NAs are marked to
the author
}
\seealso{\code{\link{createlfobj}}}


\examples{
#Finding the sample file on your computer
path <- system.file("samplesheets/9104020.day", package = "lfstat")

grdc <- readlfdata(path, type = "GRDC", baseflow = FALSE)
#Some test...
head(grdc)
}

