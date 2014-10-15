\name{digits}
\alias{allDigits}
\alias{isNumeric}
\alias{xToBase}
\title{Test, convert numbers}
\description{
 Test, convert numbers
}
\usage{
allDigits( str )
isNumeric(str)
xToBase( x, base=2 )
}
\arguments{
  \item{base}{Integer}
  \item{x}{ Real}
  \item{str}{String}
}
\details{
  \code{alldigits} Convert a string to a number literally.\cr
  \code{isNumeric} Test whether the elements of a character vector
	represent legal numbers only.
  \code{str2dig} Convert a string to a vector of integers.\cr
  \code{int} integer part truncate towards 0.\cr
  \code{xToBase} return \code{res$a}, \code{res$e} so that\cr
         x  = a*base^e, abs( a ) in [1, base)
}
\examples{
  allDigits(c("1231","89a8742")) #  TRUE FALSE
  isNumeric(c("1231","8.9e-2",".7d2")) # [1]  TRUE  TRUE FALSE
  str2dig("10010") # [1] 1 0 0 1 0
  int(10^(7:10)) # 10000000 100000000 1000000000 NA
  x <- 8.16617
  xToBase(x,2);xToBase(x,3);xToBase(x,4)
 # 8.16617 =  1.02077*2^3 = 2.72206*3^1 = 2.04154*4^1
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
