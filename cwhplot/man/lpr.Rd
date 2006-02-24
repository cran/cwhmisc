\name{lpr}
\alias{lpr}
\title{Print an object or plot}
\description{
  If an object name is given, that object will be printed,
otherwise the current plot will be printed (assuming a Postscript
printer)
}
\usage{
lpr(object, file="Rplotlpr.ps", ...)
}
\arguments{
  \item{object}{The object to be printed.}
  \item{file}{A file name. If missing, the current plot will be printed.}
  \item{\dots}{Additional parameters for \code{\link{dev.copy}}.}
}
\author{Ray Brownrigg \email{ray@mcs.vuw.ac.nz}}
\keyword{device}
