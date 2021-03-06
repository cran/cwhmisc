\name{coords}
\alias{toPol}
\alias{toRec}
\alias{toSph}
\alias{toXyz}
\alias{rotZ}
\alias{rotA}
\alias{rotV}
\alias{rotL}
\alias{getAp}
\alias{angle}
\alias{scprod}
\alias{vecprod}
\alias{\%v\%}
\alias{\%s\%}
\title{convert coordinates, angles, simple vector operations}
\description{
 Functions for conversion of coordinates; rotation matrices for
 post(pre)-multiplication of row(column) 3-vectors; Vector product(right handed), length of vector, angle between vectors.
}
\usage{
  toPol( x, y=0 )
  toRec( r, phi=0 )
  toSph( x, y, z )
  toXyz( r, theta, phi )
  rotZ( x, y, phi )
  rotA( phi, P=c(0,0,1) )
  rotV(v, w=c(0,0,1))
  rotL(phi,k=1,m=2,N=3)
  getAp( M )
  angle(v,w)
  scprod(v, w)
  vecprod(v, w)
  v \%v\% w
}
\arguments{
  \item{x,y,z,r,theta,phi}{Real, rectangular, spherical coordinates; x,
    y, z may be combined as c(x,y,z), and  r, theta, phi as c(r,theta,phi)}
  \item{P}{c(x,y,z), coordinates of point or projection direction \code{P-'O'}, with \code{'O' =c(0,0,0)} = origin.}
  \item{v, w}{3-vectors (x, y, z).}
  \item{N}{Order of the square rotation matrix >= 2.}
  \item{k,m}{Integers (m != k) describing the plane of rotation. m==k gives unit matrix.}
  \item{M}{3x3 rotation matrix.}
}
\value{
  \code{toPol:}  c( r, phi ), r=Mod(z), phi= Arg(z); Re(z)=x, Im(z)=y\cr
  \code{toRec:}  c( x , y  ), x=Re(z) , y=Im(z); Mod(z)=r, Arg(z)=phi\cr
  \code{toSph:}   c(r, theta, phi), r=sqrt(x^2+y^2+z^2), theta=atan2(z,v),\cr
                   phi=atan2(y,x) ; v=sqrt(x^2+y^2)\cr
  \code{toXyz:}   c(x, y, z), x=r*sin(phi)*sin(theta), y=r*cos(phi)*sin(theta), z=r*cos(theta)\cr
  \code{rotZ:}    c(x', y') = rotated (x, y) by angle phi, counter clockwise,\cr
  -- Rotation matrices:\cr
  \code{rotA}: Ratation matrix to rotate around axis \code{P - 'O'}.\cr
  \code{rotV}: Ratation matrix to rotate \code{v}  into \code{w}.\cr
  \code{rotL}: Matrix \code{m} for multiplication \code{m \%*\% vector}.\cr
  \code{getAp}: List with rotation axis and rotation angle corresponding to input matrix.\cr
  -- Other:\cr
  \code{angle} angle between vectors\cr
  \code{lV} Euclidean (spatial) length of vector\cr
  \code{scprod } scalar product\cr
  \code{vecprod} vector product = cross product
}
\details{
  \code{toPol,toRec}: Convert plane rectangular c(x,y) <-> polar c(r,phi); phi = angle(x-axis,point).\cr

  \code{toSph, toXyz:} Rectangular c(x,y,z) <-> spherical coordinates c(r,theta,phi); theta = angle(z-axis,P-'O'), phi= angle[plane(P,z-axis), plane(x-z)].
}
\examples{
  pkg <- TRUE # FALSE for direct use
  (x <- toPol(1.0, 1.0) ) # $r 1.41421, $p 0.785398 = pi/4
  (y <- toRec(2.0,pi) ) # $x -2, $y 2.44921e-16
  toPol(y[1], y[2]) # 2, pi
  toRec( x[1], x[2]) # 1, 1
  rotZ( 1, 0, pi/2 )  # 6.123032e-17 1.000000e+00
  x <- 1;  y <- 2;  z <- 3
  (R <- toSph(c(x,y,z)) )  # r= 3.7416574, theta= 0.64052231, phi= 1.1071487
  c(R[1],180/pi*(R[2:3])) #  3.741657 36.6992252 63.434949
  (w <- toXyz(R[1], R[2], R[3]))  #  = x,y,z
  rotZ(1,2,pi/2)  # -2, 1
  opar <- par(mfrow=c(2,4))
  x <- seq(0,1,0.05)
  phi <- c(pi/6,pi/4,-pi/6)
  Data <- matrix(c(x^2*10,(x^2-10*x)*4,(x+10)*1.5),ncol=3)
##  Data <- matrix(c(rnorm(99)*10,rnorm(99)*4,rnorm(99)*1.5),ncol=3)
  lim <- range(c(Data,-Data))*1.5
  RD <-  Data \%*\% rotL(phi[1],1,2)   # !! # rotate around z-axis
  RD2 <- RD \%*\% rotL(phi[2],2,3) # !! # rotate further around x
  RD3 <- RD2 \%*\% rotL(phi[3],1,2) # !! # rotate back around z
 \dontrun{
  plot(Data[,-3],xlim=lim,ylim=lim,xlab="x",ylab="y",pty="s")
  plot(RD[,-3],xlim=lim,ylim=lim,xlab="RD x",ylab="y",pty="s",pch=5,col="red")
  plot(RD2[,-3],xlim=lim,ylim=lim,xlab="RD2 x",ylab="y",pch=6,col="blue")
  plot(RD3[,-3],xlim=lim,ylim=lim,xlab="RD3 x",ylab="RD3 y",col="magenta")
  plot(Data[,1],RD3[,1])
  plot(Data[,2],RD3[,2])
  plot(Data[,3],RD3[,3])
 }
  m <- rotL(phi[1],1,2) \%*\% rotL(phi[2],2,3) \%*\% rotL(phi[3],1,2) # !! #
  if (pkg) {
    m <- rotL(phi[1],1,2) \%*\% rotL(phi[2],2,3) \%*\% rotL(phi[3],1,2) # !! #
    round(m \%*\% t(m),2) #!! # composite rotation matrix and orthogonality,
       # should be diag(3)
  } else {
    m <- rotL(phi[1],1,2) \%*\% rotL(phi[2],2,3) \%*\% rotL(phi[3],1,2) # !! #
    round(m \%*\% t(m),2) #!! # composite rotation matrix and orthogonality,
       # should be diag(3)
  }
  eye <- c(0.5,2.5,4)
  re  <- rotV(eye)
  getAp(re) #$A [1] -9.805807e-01  1.961161e-01 -1.193931e-16
# $phi [1] 0.5674505
  round(rotA(pi/1.5, c(1,1,1)),2)  # 60 degrees around octant bisector
# [1,]    0    1    0  is permutation of axes 1 -> 2 -> 3 -> 1
# [2,]    0    0    1
# [3,]    1    0    0
}
\note{
  \code{rotZ}: see \code{\link{toPol}}
  \code{angle}: uses \code{\link{acos}} and \code{\link{asin}}\cr
  \code{  v \%v\% w }: same as \code{ vecprod(v, w)}\cr
  \code{  v \%s\% w }: same as \code{ scprod(v, w)}
 
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
