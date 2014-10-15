\name{rotL}
\alias{rotA}
\alias{rotV}
\alias{rotL}
\alias{getAp}
\title{Rotation matrices}
\description{
 Rotation matrices for post(pre)-multiplication of row(column) 3-vectors, e.g. for rotation around an axis.
}
\usage{
  rotA( phi, P=c(0,0,1) )
  rotV(v, w=c(0,0,1))
  rotL(phi,k=1,m=2,n=3)
  getAp( M )
}
\arguments{
  \item{P}{c(x,y,z), coordinates of point or projection direction \code{P-O}, with \code{O =c(0,0,0)}.}
  \item{phi}{Real, angle of counter clockwise rotation in radian.}
  \item{v, w}{3-vectors (x, y, z).}
  \item{n}{Order of the square rotation matrix >= 2.}
  \item{k,m}{Integers (m != k) describing the plane of rotation. m=k gives Unit matrix.}
  \item{M}{3x3 rotation matrix.}
}
\value{
  rotation matrices\cr
  \code{rotA}: Ratation matrix to rotate around axis \code{P - O}.\cr
  \code{rotV}: Ratation matrix to rotate \code{v}  into \code{w}.\cr
  \code{rotL}: Matrix \code{m} for multiplication \code{m\%*\%vector}.\cr
  \code{getAp}: List with rotation axis and rotation angle corresponding to input matrix.\cr
}
\details{
   \code{rotL} with  m == k will \emph{ stop} with an \emph{error}.
}
\note{
  \code{rotZ} see \code{\link{toPol}}
}
\examples{
  opar <- par(mfrow=c(2,4))
  x <- seq(0,1,0.05)
  phi <- c(pi/6,pi/4,-pi/6)
  Data <- matrix(c(x^2*10,(x^2-10*x)*4,(x+10)*1.5),ncol=3)
##  Data <- matrix(c(rnorm(99)*10,rnorm(99)*4,rnorm(99)*1.5),ncol=3)
  lim <- range(c(Data,-Data))*1.5
  RD <-  Data \%*\% rotL(phi[1],1,2)   # !! # rotate around z-axis
  RD2 <- RD \%*\% rotL(phi[2],2,3) # !! # rotate further around x
  RD3 <- RD2 \%*\% rotL(phi[3],1,2) # !! # rotate back around z
  plot(Data[,-3],xlim=lim,ylim=lim,xlab="x",ylab="y",pty="s")
  plot(RD[,-3],xlim=lim,ylim=lim,xlab="RD x",ylab="y",pty="s",pch=5,col="red")
  plot(RD2[,-3],xlim=lim,ylim=lim,xlab="RD2 x",ylab="y",pch=6,col="blue")
  plot(RD3[,-3],xlim=lim,ylim=lim,xlab="RD3 x",ylab="RD3 y",col="magenta")
  plot(Data[,1],RD3[,1])
  plot(Data[,2],RD3[,2])
  plot(Data[,3],RD3[,3])
  m <- rotL(phi[1],1,2) \%*\% rotL(phi[2],2,3) \%*\% rotL(phi[3],1,2) # !! #
  round(m \%*\% t(m),2) #!! # composite rotation matrix and orthogonality, should be diag(3)
  eye <- c(0.5,2.5,4)
  re  <- rotV(eye)
  getAp(re) #$A [1] -9.805807e-01  1.961161e-01 -1.193931e-16
# $phi [1] 0.5674505
  round(rotA(pi/1.5, c(1,1,1)),2)  # 60 degrees around octant bisector
# [1,]    0    1    0  is permutation of axes 1 -> 2 -> 3 -> 1
# [2,]    0    0    1
# [3,]    1    0    0

}
\author{ Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{algebra}
