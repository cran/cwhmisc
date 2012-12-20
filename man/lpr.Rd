\name{lpr}
\alias{lpr}
\alias{pdfc}
\title{Print an object or plot}
\description{
  \code{lpr}If an object name is given, that object will be printed,
otherwise the current plot will be printed. \code{pdfc} The current plot will be  printed (assuming a Postscript printer)
}
\usage{
lpr(object, file="Rplotlpr.ps", ...)
pdfc(file = "Rplot.pdf", ...)
}
\arguments{
  \item{object}{The object to be printed. If missing, the current plot will be printed.}
  \item{file}{A file name}
  \item{\dots}{Additional parameters for \code{\link{dev.copy}}.}
}
\author{Ray Brownrigg \email{ray@mcs.vuw.ac.nz} \cr
modified by Christian W. Hoffmann  \email{c-w.hoffmann@sunrise.ch}
}
\keyword{device}
