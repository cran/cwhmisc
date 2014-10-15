\name{vector}
\alias{angle}
\alias{lV}
\alias{vecprod}
\alias{\%v\%}
\title{simple vector operations}
\description{
Vector product(right handed), length of vector, angle between vectors.
}
\usage{
  angle(v,w)
  lV( v )
  vecprod(v, w)
  v \%v\% w
}
\arguments{
  \item{v, w}{3-vectors (x, y, z).}
}
\value{
 \code{  v \%v\% w } same as \code{ vecprod(v, w)}
}
\examples{
tau <- (1+sqrt(5))/2  # 1.618034
a <- c(1/tau,tau,0) # vertex of dodecahedron next to y-axis
b <- c(0,1,0)  # y-axis
deg( angle(a,b))  #  20.905157 Degrees
vecprod(a, b)  # 0 0 0.61803399
}
\note{ \code{angle} uses \code{\link{acos}} and \code{\link{asin}}
}
\author{ Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{algebra}
