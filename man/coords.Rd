\name{coords}
\alias{toPol}
\alias{toRec}
\alias{toSph}
\alias{toXyz}
\alias{rotZ}
\title{convert coordinates, angles}
\description{
 Functions for conversion of coordinates, computation of sphericals triangles
}
\usage{
  toPol( x, y=0 )
  toRec( r, phi=0 )
  toSph( x, y, z, up=TRUE )
  toXyz( r, theta, phi, up=TRUE )
  rotZ( x, y, phi )
}
\arguments{
  \item{x,y,z,r,theta,phi}{Real, rectangular, spherical coordinates}
  \item{up}{if FALSE/TRUE: theta is measured from z-axis/from x-y plane}
}
\value{
  \code{toPol:}  c( r, phi ), r=Mod(z), phi= Arg(z); Re(z)=x, Im(z)=y\cr
  \code{toRec:}  c( x , y  ), x=Re(z) , y=Im(z); Mod(z)=r, Arg(z)=phi\cr
  \code{toSph:}   c(r, theta, phi), r=sqrt(x^2+y^2+z^2), theta=atan2(z,v),\cr
                   phi=atan2(y,x) ; v=sqrt(x^2+y^2)\cr
  \code{toXyz:}   c(x, y, z), x=r*sin(phi)*sin(theta), y=r*cos(phi)*sin(theta),\cr
                  z=r*cos(theta)\cr
  \code{rotZ:}    c(x', y') = rotated (x, y) by angle phi, counter clockwise
}
\details{
  \code{toPol,toRec}: Convert plane rectangular c(x,y) <-> polar coordinates\cr
                   c(r,phi), phi = angle(x-axis,point).\cr

  \code{toSph, toXyz:} Rectangular c(x,y,z) <-> spherical coordinates \cr
                  c(r,theta,phi), theta = angle(z-axis,P-O), 
                  phi= angle[plane(P,z-axis), plane(x-z)].\cr
\cr

}
\examples{
  x <- toPol(1.0, 1.0) # $r 1.41421, $p 0.785398 = pi/4
  y <- toRec(2.0,pi) # $x -2, $y 2.44921e-16
  toPol(y[1], y[2]) # 2, pi
  toRec( x[1], x[2]) # 1, 1
  rotZ( 1, 0, pi/2 )  # 6.123032e-17 1.000000e+00
  x <- 1;  y <- 2;  z <- 3
  (R <- toSph(x,y,z))  # r= 3.7416574, theta= 0.93027401, phi= 1.1071487
  c(R[1],180/pi*(R[2:3])) #  3.741657 53.300775 63.434949
  (w <- toXyz(R[1], R[2], R[3]))  #  = x,y,z
  rotZ(1,2,pi/2)  # -2, 1
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
