\name{signp}
\alias{signp}
\title{Sign Function}
\description{
  \code{sign} returns a vector with the signs of the corresponding
	elements of x (the sign of a real number is 1, 1, or -1 if the number is positive, zero, or negative, respectively). 

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
\author{Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{arith}

