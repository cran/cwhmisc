\name{signp}
\alias{signp}
\title{Sign Function}
\description{
  \code{sign} returns a vector with the signs of the corresponding
	elements of x: ifelse(x >= 0, 1, -1) . 

Note that sign does not operate on complex vectors. 
  
}
\usage{
  signp(x)
}
\arguments{
  \item{x}{a numeric vector}
}
\seealso{\code{\link{sign}}}
\examples{
signp(-2:3)# -1 -1 1 1 1 1
(m <- matrix(rnorm(9),3,3))
m \%*\% diag(signp(diag(m))) 
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{arith}

