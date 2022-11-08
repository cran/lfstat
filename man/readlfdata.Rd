\name{readlfdata}
\alias{readlfdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads data sheets
}
\description{
Reads data sheets of different formats directly and returns objects of class \code{'lfobj'}.
}
\usage{
readlfdata(file, type = c("GRDC", "HZB", "LFU", "TU"), lfobj = TRUE,
           readmeta = TRUE, encoding = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The name of the file which the data are to be read from.}
  \item{type}{The style of the sheet, currently the following formats
are accepted: \code{'GRDC'}, \code{'HZB'} (Austria), \code{'LFU'} (Germany, Bavaria), \code{'TU'}
(Technical University Vienna)}
  \item{lfobj}{logical, should a low flow object be created?}
  \item{readmeta}{logical, should meta information from data sheets be
    saved?}
  \item{encoding}{The name of the encoding to be assumed. See the Encoding section of \code{\link{connections}}.}
  \item{...}{Handed to \code{'createlfobj'}, could be \code{'hyearstart'}, \code{'baseflow'} or
  \code{'meta'}, if \code{'readmeta'} is \code{'FALSE'}.}
}
\value{
A \code{'lfobj'} or \code{'data.frame'} depending on \code{'lfobj'}.
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
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
# Finding the filename of the sample file on your computer
fn <- system.file("samplesheets/9104020.day", package = "lfstat")
grdc <- readlfdata(fn, type = "GRDC", baseflow = FALSE, hyearstart = 1)
head(grdc)


fn <- system.file("samplesheets/kloesterle.dat", package = "lfstat")
hzb <- readlfdata(fn, type = "HZB", baseflow = FALSE, hyearstart = 1)
head(hzb)

fn <- system.file("samplesheets/oberammergau.dat", package = "lfstat")
lfu <- readlfdata(fn, type = "LFU", baseflow = FALSE, hyearstart = 1)
head(lfu)
}
