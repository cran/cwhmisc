\name{astroGeo}
\alias{LB2MK}
\alias{LB2YX}
\alias{YX2LB}
\alias{YX2MK}
\alias{LongBerne}
\alias{LatBerne}
\alias{xToNorthBerne}
\alias{yToEastBerne}

\title{Convert geographical coordinates to and from Swiss topo coordinates}
\description{
 Geographic and Swiss topo rectangular coordinates,  X positive to the north,  Y positive to the east (!)
}
\usage{
LB2MK( long, lat )
LB2YX( long, lat )
YX2LB( yToEast, xToNorth )
YX2MK( yToEast, xToNorth )
}
\arguments{
  \item{long,lat, yToEast, xToNorth}{Real}
}
\details{
  \code{LB2MK} From geogr. longitude and latitude to planar meridian convergence [gon].\cr
  \code{LB2YX} From geogr. longitude and latitude to Swiss coordinates.\cr
  \code{YX2LB} From Swiss coordinates  to geogr. longitude and latitude.\cr
  \code{YX2MK} From Swiss coordinates North and East to planar meridian convergence [gon].\cr
  \code{LongBerne, LatBerne} geogr. coordinates of Berne, 7deg26'22.50" east, 46deg57'08.66" north.\cr
  \code{yToEastBerne, xToNorthBerne} Swiss topo coordinates of refernce point near Berne.
}

\examples{
LB2MK( LongBerne, LatBerne) #  7.21188e-16 [gon]
LB2MK( 9.132582913360895, 46.18669420448755) # somewhere in Switzerland , 1.37472
LB2YX( LongBerne, LatBerne) # 600.0, 200.0
YX2LB ( yToEastBerne, xToNorthBerne ) # 7.4395833  46.9524055
YX2MK ( 600, 200) #  =   0
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch> after H.Matthias, lecture 'Amtliche Vermessungswerke 1', ETH Zurich, 1986.
}
\keyword{arith}
