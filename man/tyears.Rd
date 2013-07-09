\name{tyears}
\alias{tyears}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
T-Years Event
}
\description{
Fits a mixed extreme value distribution and estimates the T-Years event.
}
\usage{
tyears(lfobj,
       event = 100,
       n = 7,
       dist = c("wei","gev","ln3","gum","pe3"),
       legend = TRUE,
       zetawei = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
An object of type "lfobj"
}
\item{event}{
  A value for T, e.g. event = 100 means the 100 years extreme low flow event
}
\item{n}{The n in the AM(n-day) series, e.g. n=7 the annual 7 day minima
  are predicted
}
\item{dist}{A vector of distribution to fit, the names are according to
  Hostking's in his "lmom" package. Use ?cdf... (... is the
  distribution-name, e.g. ?cdfwei) for more information.}
\item{legend}{
  logical, should R add a legend to the plot?}
\item{zetawei}{Manualy set the zeta parameter of the Weibull distribution}
}
\details{
According to paragraph 7.4.2 in the manual, we have to take care of
censored series. The parameters/plots are always for G(x) in (7.4), the T-Years-Event
is calculated for F(x)*.
If a Weibull distribution is fitted and zeta takes a value smaller than
0 it is set to 0. If you want to specify zeta otherwise, use the zetawei argument!
}
\value{
  A list containing
  \item{T_Years_Event}{The expected T-Years event of all used
    distributions}
  \item{pnull}{The estimated value for p_0, i.e. the estimated
    probablility for 0 as AM(n) minimum.}
  \item{parameters}{The parameters of the fitted distributions}
  \item{lmom}{The estimated L-Moments}
  
  
}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}

\seealso{
\code{\link{lmom}}, \code{\link{cdfwei}}
}
\examples{
data(ngaruroro)
#TYEARS EXAMPLE!!!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
