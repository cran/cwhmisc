\name{convert}
\alias{allDigits}
\alias{isNumeric}
\alias{intToASCII}
\alias{intToBase}
\alias{intToOct}
\alias{intToHex}
\alias{contfrac}
\alias{evalcfr}
\alias{toFrac}
\alias{str2dig}
\alias{int}
\alias{xToBase}
\alias{Hd}
\alias{Hms}
\alias{Hdms}
\alias{Hmsd}
\alias{Degree}
\alias{Radian}
\alias{ReduceArc}
\alias{ReduceArc2}
\alias{ToPolar}
\alias{ToRect}
\title{Test, convert numbers}
\description{
 Functions for testing strings, for conversion of integers to bases other than decimal as string representations
}
\usage{
allDigits( str )
isNumeric(str)
intToASCII(i)
intToBase( i, base=2 )
intToOct( i )
intToHex( i )
contfrac( x, depth = 13, f=floor ) 
evalcfr( cf )
toFrac( x, depth)
str2dig( str )
int( x )

xToBase( x, base=2 )
int( x )
Hd( h, m, s )
Hdms( hd )
Hmsd( hms )
Degree( radian )
Radian( degree )
ReduceArc( U, ref )
ReduceArc2(U, V, ref )
ToPolar( x, y )
ToRect( r, p )
}
\arguments{
  \item{i,base,depth}{Integer}
  \item{x,y,r,p,h,m,s,hd,hms,U,V,ref,radian,degree}{Real}
  \item{f}{function to use, normally 'floor', otherwise 'round' or 'trunc'}
  \item{cf}{Vector of integers representing the continued fraction of areal number}
  \item{str}{String}
}
\details{
  \code{alldigits} Convert a string to a number literally.\cr
  \code{isNumeric} Test whether the elements of a character vector
	represent legal numbers only.
  \code{intToASCII} Show character or octal representation at a place in the ASCII sequence.\cr
  \code{intToBase} Convert an integer number to string representation in a base between 2 and 16 inclusive.\cr
  \code{intToOct} Convert integer to octal representation.\cr
  \code{intToHex} Convert integer to hex representation.\cr
  \code{contfrac} Convert to continued fraction representation.\cr
  \code{evalcfr} Evaluate continued fraction to corresponding real.\cr
  \code{toFrac} Build rational approximation \code{num/den} to \code{x} using forward continued fraction recursion to a depth of \code{depth}. Stopping criterion: either \code{depth} is reached, or abs(x - num/den) is increasing again.\cr
  \code{str2dig} Convert a string to a vector of integers.\cr
  \code{int} truncate towards 0.\cr
  \code{xToBase} return \code{res$a}, \code{res$e} so that  x  = a*base^e, abs( a ) in [1, base)\cr
  \code{sqr} x^2.\cr
  \code{Hd} Return hours from h, min, sec.\cr
  \code{Hms} Return h, min, sec from hours, is inverse of Hd.\cr
  \code{Hdms} Return hh.mmss from hours.\cr
  \code{Hmsd} Decimal hours from hours.mmss, is inverse of Hdms.\cr
  \code{Degree} Convert  radians  to  degrees.\cr
  \code{Radian} Convert  degrees  to  radians.\cr
  \code{ReduceArc} Add or subtract multiples of ref to make  \eqn{abs(U) < ref/2}.\cr
  \code{ReduceArc2} Subtract from \code{U} and \code{V} the greatest multiple of ref, so that \eqn{0 <= min {U_new, V_new} < ref}.\cr
  \code{ToPolar,ToRect} Convert rectangular into polar coordinates and vice versa. Further explanation in function code.
}
\examples{
allDigits(c("1231","89a8742")) #  TRUE FALSE
isNumeric(c("1231","8.9e-2",".7d2")) # [1]  TRUE  TRUE FALSE
intToASCII(1:255)[121:129] # "x"   "y"   "z"   "{"   "|"   "}"   "~"   "\177" "\200"
sapply(1:50,intToBase,2)
sapply(1:50,intToBase,7)
sapply(1:50,intToOct)
sapply(1:50,intToHex)
(pcf <- contfrac(pi)) # 3, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, (1)
## last integer incorrect due to rounding errors
evalcfr(pcf)-pi # 0
str2dig("10010") # [1] 1 0 0 1 0
int(10^(7:10)) # 10000000 100000000 1000000000 NA
gcd(35,133) # 7
x <- exp(2.1)
xToBase(x,2);xToBase(x,3);xToBase(x,4) # 8.16617 =  1.02077*2^3 = 2.72206*3^1 = 2.04154*4^1
Hd( 12,25,17) # 12.421389
Hms(1.421389) # $h  1, $m 25, $s 17.0004
Hmsd(1.421389) # 1h 42m 13.89 -> 1.703858
Hdms(1.703858) # 1.421389
Degree(pi/2)  # 90
Radian(180)  #  3.141593
ReduceArc(580,360) # -140
ReduceArc2(200,120,70) # 130, 50
ReduceArc2(100,-200,70) # 310, 10
ToPolar(1.0, 1.0) # $r 1.41421, $p 0.785398
ToRect(2.0,pi) # $x -2, $y 2.44921e-16
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{character,arith}