\name{lengths.angle}
\alias{lengths.angle}
\title{Lengths of two vectors and angle between them.}
\description{
  Compute lengths of two vectors and the angle between them.
}
\usage{
  lengths.angle(x, y)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{the first vector. Alternatively, a single matrix with both
		vectors as \emph{columns} can be provided.}
  \item{y}{the second vector, \emph{optional} if \code{x} is an appropriate structure.}
}
\value{
  A list with elements
  \item{lx}{the length of \code{x}.}
  \item{ly}{the length of \code{y}.}
  \item{angle}{the angle (in radian) between \code{x} and \code{y}.}
  \item{angleDeg}{the angle (in degrees) between \code{x} and \code{y}.}
}
\examples{
  x <- c(1,2,2,-1)
	y <- c(0,0.1,3,2)
  (l <- lengths.angle(x, y))  # == lengths.angle(cbind(x, y))
#> $lx    #> [1] 3.16228
#> $ly    #> [1] 3.60694
#> $angle #> [1] 1.1937
l$angle/pi*180 # in degrees  >  68.394
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{algebra}

