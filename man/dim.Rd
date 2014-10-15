\name{Dim}
\alias{Dim}
\title{dim of vectors and arrays}
\description{Get length of vectors and dimension of arrays in a unified manner.
}
\usage{
  Dim(x)
}
\arguments{
  \item{x}{vector or array}
}
\value{
  Integer vector containing length of vector or dimension of array.
}
\examples{
  x <- matrix(1:12,3,4)
  Dim(x)  #   3 4
  y <- rep(0,5)
  Dim(y)  # 5
}
\author{Christian W. Hoffmann, \email{christian@echoffmann.ch}}
\keyword{structures}


