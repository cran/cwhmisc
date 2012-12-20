\name{eql}
\alias{eql}
\title{Check on equality, including NA==NA and NaN==NaN.}
\description{
  \code{my.table} checks two vectors on equality, two NA's and two NaN's
	compare as equal.
}
\usage{
eql(x, y)
}
\arguments{
  \item{x, y}{vectors of equal length.}
}
\value{
	A vector of logicals indicating the result of the element by element comparison.The elements of shorter vectors are recycled as necessary. 
}
\examples{
  eql(c(1,2,3),c(1,3)) #>  TRUE FALSE FALSE
  eql(c(1,2,3),c(1,2)) #>  TRUE  TRUE FALSE
  eql(c(NA,NaN,2,NA,3),c(NA,NaN,1,2,3)) #>  TRUE  TRUE FALSE FALSE
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>, \cr
idea by Peter Dalgaard, \email{p.dalgaard@biostat.ku.dk} }
\keyword{arith}
