\name{trace_value}
\alias{trace_value}
\title{Draw Paths to Points perpendicular to Coordinate Axis}
\description{
To depict the distances in \code{x} and \code{y} direction to a point, draw lines and labels.}
\usage{
trace_value(x, y, digits = 0, annotate = TRUE, lab.x = x, lab.y = y, prefix = "",
            suffix = "", cex = 0.75, col = "blue", lty = 2, ...)
}
\arguments{
  \item{x}{numeric vector of x coordinates}
  \item{y}{numeric vector of y coordinates}
  \item{digits}{vector of length one or two, giving the number of digits used for rounding the label of the \code{x} and \code{y} coordinate.}
  \item{annotate}{logical, should the lines get annotated with labels?}
  \item{lab.x}{character vector of length one. Label of the \code{x} coordinate.}
  \item{lab.y}{character vector of length one. Label of the \code{y} coordinate.}
  \item{prefix}{vector of length one or two, text printed before the label of the \code{x} and \code{y} coordinate.}
  \item{suffix}{vector of length one or two, text printed after the label of the \code{x} and \code{y} coordinate.}
  \item{cex}{character expansion factor}
  \item{col}{color used for text and lines}
  \item{lty}{line type}
  \item{\dots}{other graphical parameters, passed on to \code{lines}, \code{points} and \code{text}.}
}
\details{This function is vectorised over \code{x} and \code{y}.}

\examples{
x <- c(-2, 3)
curve(sin, -2*pi, 2*pi, xname = "t")
trace_value(x, sin(x), digits = c(0, 1))
}

\keyword{low-flow}
\keyword{aplot}

