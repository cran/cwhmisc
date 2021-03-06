\name{num.ident}
\alias{num.ident}
\title{Check numerical values for identity}
\description{
  Check two variables on numerical identity or whether both  are either NaN or NA.
}
\usage{
  num.ident(x,y)
}
\arguments{
  \item{x, y}{Variables to check for identity, may be arrays.}
}
\value{
	\code{\link{TRUE}}, \code{\link{FALSE}}
}
\note{No check is made whether \code{x} or \code{y} are numeric}
\examples{
  xxxx <- c(100,-1e-13,Inf,-Inf, NaN, pi, NA)
  names(xxxx) <- formatC(xxxx, dig=3)
  (aaaa <- outer(xxxx,xxxx,function(x,y) num.ident(x,y)))
  all((aaaa & !is.na(aaaa)) == (row(aaaa) == col(aaaa)))
  # aaaa has TRUE only on the diagonal, i.e. identity works correctly
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{logic}
\keyword{arith}
