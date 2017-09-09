\name{str2B}
\alias{str2B}
\title{round real in string}
\description{ Function for rounding real given as string representation
}
\usage{
  str2B(str, base=10, round = 0)
}
\arguments{
  \item{str}{String representing a real}
  \item{base}{1 < integer < 17, base of representation}
  \item{round}{Integer, number of places after "." to be rounded; < 0: rounding
    places before least significand digit. If too negative, 0 will result.}
}
\value{
  \code{str2B} from given string represention of \code{x}, round to 'round' decimal  digits\cr
}
\examples{
  x <- paste0("-    ","9167.8")
  str2B(x )
  for ( kk in -5:4) print(str2B(x,10,kk) )
# 0 -10000 -9000 -9200 -9170 -9168 -9167.8 ...
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{arith}
