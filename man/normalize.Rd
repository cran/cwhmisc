\name{normalize}
\alias{normalize}
\title{
base power and multiplier
}
\description{
split a number into base power and multiplier, x = a*base^e
}

\usage{
normalize( x, base=2 )
}
\arguments{
  \item{x}{Real}
  \item{base}{Base of power}
}
\details{
\code{normalize} Return c( a, e ) so that  x  = a*base^e,   abs( a ) in [1, base) \cr
}
\examples{
  normalize(100,pi)  ##  1.0265982 4.0000000
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{math}
