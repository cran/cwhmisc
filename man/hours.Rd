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
Hms( hd )
Hdms( hd )
Hmsd( hms )
}
\arguments{
  \item{h, m, s}{Real, representing hours, minutes, seconds}
  \item{ hd,hms }{Real, decimal hours, and concatenated h,m,s}
}
\value{
  \code{Hd}{hours}
  \code{Hms}{c(h,m,s)}
  \code{Hdms}{hh.mmss}
  \code{Hmsd}{(Decimal) hours}
}
\examples{
  Hd( 12,25,17) # 12.421389
  Hms(1.421389) # 1h 25m 17.0004s
  Hmsd(12.421389) # 1h 42m 13.89 -> 12.703858 h
  Hdms(12.703858) # 12.421389 h
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
