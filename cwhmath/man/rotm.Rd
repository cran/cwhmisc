\name{rotm}
\alias{rotm}
\title{Rotation matrix}
\description{
 Generate a (square) rotation matrix.
}
\usage{
rotm(n,x,y,phi)
}
\arguments{
  \item{n}{Order of the square matrix.}
  \item{x,y}{Integers describing the plane of rotation.}
  \item{phi}{Angle (counted counter clockwise) of rotation of coordinate 
	system.}
}
}
\value{
  Matrix to use for pre-multiplying.
}
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
}
\author{Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
 \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{algebra}
