\name{rotL}
\alias{rotL}
\alias{rotaP}
\alias{rotPz}
\title{Rotation matrix}
\description{
 Generate a square orthogonal rotation matrix for pre-multiplication.
}
\usage{
  rotL(phi,k=1,m=2,n=3)
  rotPz(P)
}
\arguments{
  \item{n}{Order of the square rotation matrix >= 2.}
  \item{k,m}{Integers describing the plane of rotation, i.e. 1,2,3=x,y,z}
  \item{phi}{Real, angle of counter clockwise rotation in radian.}
  \item{P}{c(x,y,z), coordinates of projection point or projection direction \code{P-Origin}}
}
\value{
  \code{rotL}: Matrix \code{m} for multiplication \code{m\%*\%vector}.\cr
  \code{rotPz}: rotation matrix for \eqn{(0,0,zz)' = r * (x,y,z)'}.\cr
  \code{rotaP}: rotation matrix around axis (P - Orig, Orig = c(0,0,0)).
}
\examples{
  par(mfrow=c(2,4))
  x<- -20:20;Data <- matrix(c(x^2*10,(x^2-10*x)*4,(x+10)*1.5),ncol=3)
##  Data <- matrix(c(rnorm(99)*10,rnorm(99)*4,rnorm(99)*1.5),ncol=3)
  lim <- range(c(Data,-Data))*1.5
  RD <- Data %*% rotL(pi/6,1,2)  # rotate around z
  RD2 <- RD %*% rotL(pi/4,2,3) # rotate further around x
  RD3 <- RD2 %*% rotL(-pi/6,1,2) # rotate back around z
  plot(Data[,-3],xlim=lim,ylim=lim,xlab="x",ylab="y",pty="s")
  plot(RD[,-3],xlim=lim,ylim=lim,xlab="RD x",ylab="y",pty="s",pch=5,col="red")
  plot(RD2[,-3],xlim=lim,ylim=lim,xlab="RD2 x",ylab="y",pch=6,col="blue")
  plot(RD3[,-3],xlim=lim,ylim=lim,xlab="RD3 x",ylab="RD3 y",col="magenta")
  plot(Data[,1],RD3[,1])
  plot(Data[,2],RD3[,2])
  plot(Data[,3],RD3[,3])
  m <- rotL(pi/6,1,2) %*% rotL(pi/4,2,3) %*% rotL(-pi/6,1,2)
  m%*%t(m) # composite rotation matrix and orthogonality
  eye <- c(0.5,2.5,4)
  re  <- rotPz(eye)
  round(rotaP(c(1,1,1),pi/1.5),2)
# [1,]    0    1    0  Permutation of axes 1 -> 2 -> 3 -> 1
# [2,]    0    0    1
# [3,]    1    0    0
}
\author{ Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{algebra}
