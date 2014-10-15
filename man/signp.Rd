\name{signp}
\alias{signp}
\title{Sign Function}
\description{
  \code{signp} returns a vector with the signs of the elements of x: ifelse(x >= 0, 1, -1), avoiding 0. 
}
\usage{
  signp(x)
}
\arguments{
  \item{x}{a numeric vector}
}
\note{
  \code{signp} does not operate on complex vectors.
}
\seealso{\code{\link{sign}}}
\examples{
signp(-1:1) # -1 1 1 instead of sign(-1:1) = -1 0 1
(m <- matrix(rnorm(9),3,3))
m \%*\% diag(signp(diag(m))) 
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{arith}

