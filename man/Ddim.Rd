\name{Ddim}
\alias{Ddim}
\title{dim of vectors and arrays}
\description{Get length of vectors and dimension of arrays in a unified manner.
}
\usage{
  Ddim(x)
}
\arguments{
  \item{x}{vector or array}
}
\value{
  Integer vector containing length of vector or dimension of array.
}
\examples{
  Ddim(matrix(1:12,3,4))  #   3 4
  Ddim(rep(0,5))  # 5
}
\author{Christian W. Hoffmann, \email{christian@echoffmann.ch}}
\keyword{structures}


