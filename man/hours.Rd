\name{hours}
\alias{Hd}
\alias{Hms}
\alias{Hdms}
\alias{Hmsd}
\title{convert hours}
\description{
 Functions for conversion of hour representations
}
\usage{
Hd( h, m, s )
Hdms( hd )
Hmsd( hms )
}
\arguments{
  \item{h, m, s, hd,hms}{Real, representing hours, minutes, seconds, decimal hours, and concatenated h,m,s}
}
\details{
  \code{Hd} Return hours from h, min, sec.\cr
  \code{Hms} Return h, min, sec from hours, is inverse of Hd.\cr
  \code{Hdms} Return hh.mmss from hours.\cr
  \code{Hmsd} Decimal hours from hours.mmss, is inverse of Hdms.\cr
}
\examples{
Hd( 12,25,17) # 12.421389
Hms(1.421389) # $h  1, $m 25, $s 17.0004
Hmsd(1.421389) # 1h 42m 13.89 -> 1.703858
Hdms(1.703858) # 1.421389
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
