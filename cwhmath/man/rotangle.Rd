\name{rotangle}
\alias{rotangle}
\title{Compute rotation angles from orthogonal matrix.}
\description{
  Decompose the orthogonal matrix \code{Q} into the product \code{R3 * R2 * R1} where the \code{Ri} are the elementary rotations around the i-th axis.
}
\usage{
  rotangle(Q)
}
\arguments{
  \item{Q}{Orthogonal matrix.}
}
\value{
  The vector containing the rotation angles [radian]
}
\references{"Least squares fit of point clouds" in: W. Gander and J. Hrebicek, ed., Solving Problems in Scientific Computing using Maple and Matlab, Springer Berlin Heidelberg New York, 1993, third edition 1997.}
\author{Walter Gander, \email{gander@inf.ethz.ch}, \cr
 \url{http://www.inf.ethz.ch/personal/gander/}}
\seealso{ \code{\link{pointfit}}, \code{\link{rotm}} for synthesizing
 such a matrix.}
\examples{
  rot <- matrix(c(0.847101, -0.480873, -0.226234, 0.489074,  0.538865,
0.685880, -0.207912, -0.691655,  0.691655),3,3,byrow=TRUE)
  rotangle(rot) #> 0.785398  0.209440 -0.523599
  rotangle(rot)/pi*180 #> degrees:  45  12 -30
}
\keyword{algebra}

