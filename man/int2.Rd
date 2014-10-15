\name{int2}
\alias{str2dig}
\alias{intToASCII}
\alias{intToBase}
\alias{intToOct}
\alias{intToHex}
\title{convert integers, string to integer wector}
\description{
 Functions for conversion of integers to bases other than decimal
}
\usage{
str2dig(str)
intToASCII(i)
intToBase( i, base=2 )
intToOct( i )
intToHex( i )
}
\arguments{
  \item{str}{String}
  \item{i,base}{Integer}
}
\details{
  \code{str2dig} Convert literally a string to a vector.\cr
  \code{isNumeric} Test whether the elements of a character vector
	represent legal numbers only.
  \code{intToASCII} Show character or octal representation at a place in the ASCII sequence.\cr
  \code{intToBase} Convert an integer number to string representation in a base between 2 and 16 inclusive.\cr
  \code{intToOct} Convert integer to octal representation.\cr
  \code{intToHex} Convert integer to hex representation.\cr
  \code{contfrac} Convert to continued fraction representation.\cr
  \code{evalcfr} Evaluate continued fraction to corresponding real.\cr
  \code{toFrac} Build rational approximation \code{num/den} to \code{x} using forward continued fraction recursion to a depth of \code{depth}. Stopping criterion: either \code{depth} is reached, or abs(x - num/den) is increasing again.\cr
  \code{toFrac2} same as \code{toFrac}, but vectors of partial numerators and denominators are returned.\cr
  \code{xToBase} return \code{res$a}, \code{res$e} so that  x  = a*base^e, abs( a ) in [1, base)\cr
}
\examples{
str2dig("10010") # [1] 1 0 0 1 0
intToASCII(1:255)[121:129] # "x"   "y"   "z"   "{"   "|"   "}"   "~"   "\177" "\200"
sapply(1:50,intToBase,2)
sapply(1:50,intToBase,7)
sapply(1:50,intToOct)
sapply(1:50,intToHex)
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{arith}
