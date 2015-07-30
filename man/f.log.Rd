\name{f.log}
\alias{f.log}
\title{Determine an optimized offset s and return log10(data+s).}
\description{
  \code{f.log} determines a positive offset \code{s} for zero values to
  be used in a subsequent log transformation.
}
\usage{
  f.log(x)
}
\arguments{
  \item{x}{vector of data.}
}
\value{
  The transformed values log10(data + s).
  }
  
\examples{
  x <- c(rep(0,20), exp(rnorm(1000,0.05)))
  fx <- f.log(x)
 \dontrun{
  oldpar <- par(mfrow = c(2, 3))
  plot(x,main="exp(normal)+zeros")
  qqnorm(x)
  T3plot(x)
  plot(fx,main="optimized offset")
  qqnorm(fx)
  T3plot(fx)
  par(oldpar)
 }
}
\note{
  The value for the offset \code{s} is optimized to render the transformed
  values of \code{x} log-normal
  }
\author{W.Stahel, ETH Zuerich, \email{werner.stahel@stat.math.ethz.ch}
adapted by:
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{distribution}
