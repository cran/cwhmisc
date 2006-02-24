\name{f.log}
\alias{f.log}
\title{Determine an optimized offset s and return log10(data+s).}
\description{
  \code{f.log} determines a positive offset s for zero values to
  be used in a subsequent log transforamtion.
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
  x <- c(rep(0,20), exp(rnorm(1000)))
  fx <- f.log(x)
  oldpar <- par(mfrow = c(2, 2))
  plot(x)
  plot(fx)
  qqnorm(x)
  qqnorm(fx)
  par(oldpar)
}
\note{
  The value for the offset s is optimized to render ...
  }
\author{W.Stahel, ETH Zuerich, \email{werner.stahel@stat.math.ethz.ch}}
\keyword{distribution} 