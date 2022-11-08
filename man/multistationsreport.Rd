\name{multistationsreport}
\alias{multistationsreport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Report for several stations
}
\description{
Calculates indices for several stations at once.
}
\usage{
multistationsreport(...,indices = c("meanflow", "Q95", "MAM1", "MAM7",
    "MAM10", "MAM30", "MAM90", "baseflowindex", "recession"),
    recessionmethod = "MRC", recessionseglength = 7, recessionthreshold = 70,
    recessiontrimIRS = 0.1,lflist = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{Objects of class \code{"lfobj"}}
  \item{indices}{A vector of indices to calculate}
  \item{recessionmethod}{See \code{\link{recession}}}
  \item{recessionseglength}{See \code{\link{recession}}}
  \item{recessionthreshold}{See \code{\link{recession}}}
  \item{recessiontrimIRS}{See \code{\link{recession}}}
  \item{lflist}{Alternative give a list containing low flow objects.}
}
\value{
A \code{data.frame} containing the calculated indices.
}

\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, \acronym{WNO}-No. 1029, 136p.
}

\author{
Daniel Koffler and Gregor Laaha
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{\code{\link{meanflow}}, \code{\link{Q95}},\code{\link{MAM}},\code{\link{BFI}},\code{\link{recession}}}


\examples{
data(ngaruroro)
multistationsreport(ngaruroro, indices = c("meanflow", "MAM7"))

seventies <- subset(ngaruroro, hyear \%in\% 1970:1979)
eighties <- subset(ngaruroro, hyear \%in\% 1980:1989)
nineties <- subset(ngaruroro, hyear \%in\% 1990:1999)

multistationsreport(seventies, eighties, nineties)
}

