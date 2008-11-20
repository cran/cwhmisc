\name{rotm}
\alias{rotm}
\alias{rotangle}
\title{Rotation matrices and compute rotation angles from orthogonal matrix}
\description{
 Generate a (square) rotation matrix.
  Decompose the orthogonal matrix \code{Q} into the product \code{R3 * R2 * R1} where the \code{Ri} are the elementary rotations around the i-th axis.
}
\usage{
  rotm(n,x,y,phi)
  rotangle(Q)
}
\arguments{
  \item{n}{Order of the square matrix.}
  \item{x,y}{Integers describing the plane of rotation.}
  \item{phi}{Angle (counted counter clockwise) of rotation of coordinate 
	system.}
  \item{Q}{Orthogonal matrix.}
}
\value{
  \code{rotm}: Matrix to use for pre-multiplying.
  \code{rotangle}: The vector containing the rotation angles [radian]
}
\seealso{ \code{\link{pointfit}}, \code{\link{rotm}} for synthesizing
 such a matrix.}
\examples{
  par (mfrow=c(2,2))
  Data <- rbind(rnorm(100)*100,rnorm(100)*40,rnorm(100)*5,rnorm(100)*1,rnorm(100)*0.01)
  Rotdata <- rotm(dim(Data)[1],1,2,pi/3) %*% Data
  Rotdata2 <- rotm(dim(Data)[1],2,4,pi/5) %*% Rotdata
  plot(Data[1,],Data[2,])
  plot(Rotdata[1,],Rotdata[2,],col="red")
  points(Rotdata2[1,],Rotdata2[2,],col="blue")
  plot(Rotdata2[2,],Rotdata2[3,],col="red")
  plot(Rotdata2[4,],Rotdata2[3,],col="red")
  rot <- matrix(c(0.847101, -0.480873, -0.226234, 0.489074,  0.538865,
0.685880, -0.207912, -0.691655,  0.691655),3,3,byrow=TRUE)
  rotangle(rot) #> 0.785398  0.209440 -0.523599
  rotangle(rot)/pi*180 #> degrees:  45  12 -30
}
\references{
"Least squares fit of point clouds" in: W. Gander and J. Hrebicek, ed., Solving Problems in Scientific Computing using Maple and Matlab, Springer Berlin Heidelberg New York, 1993, third edition 1997.
}
\author{
  \code{rotangle}: Walter Gander, \email{gander@inf.ethz.ch}, \cr
 \url{http://www.inf.ethz.ch/personal/gander/} \cr
adapted by:
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{algebra}
