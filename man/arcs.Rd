\name{arcs}
\alias{deg}
\alias{rad}
\alias{reda}
\alias{reda2}
\title{Convert and reduce arcs}
\description{
 Functions for reduction of arcs.
}
\usage{
deg( radian )
rad( degree )
reda( U, ref )
reda2(U, V, ref )
}
\arguments{
  \item{U,V,ref,radian,degree}{Real}
}
\details{
  \code{deg} Convert  radians  to  degrees.\cr
  \code{rad} Convert  degrees  to  radians.\cr
  \code{reda} Add or subtract multiples of ref to make  \eqn{abs(U) < ref/2}.\cr
  \code{reda2} Subtract from \code{U} and \code{V} the greatest multiple of ref, so that \eqn{0 <= min {U_{new}, V_{new}} < ref}.\cr
}
\examples{
deg(pi/2)  # 90
rad(180)  #  3.141593
reda(580,360) # -140
reda2(200,120,70) # 130, 50
reda2(100,-200,70) # 310, 10
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}