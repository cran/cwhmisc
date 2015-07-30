\name{functions}
\alias{inrange}
\alias{mod}
\alias{modS}
\alias{modR}
\alias{zero}
\alias{one}
\alias{onebyx}
\alias{sqr}
\alias{powr}
\alias{equal}
\alias{equalFuzzy}
\alias{quotmean}
\alias{safeDiv}
\alias{solveQeq}
\alias{chsvd}
\alias{divmod}
\alias{submod}
\alias{dsm}
\title{Functions for testing and other}
\description{
 Functions for testing on equality, exactly or with a tolerance, functions usable as parameters in other functions.
}
\usage{
inrange(x,r)
mod( m, n  )
modS( m, n )
modR( x, y )
one ( x )
zero ( x )
onebyx( x )
sqr( x )
powr( a, x )
equal( x, y )
equalFuzzy( x, y, prec=8*.Machine$double.eps, rel=TRUE )
quotmean(x,y)
safeDiv( num, den )
solveQeq(a,b,c)
chsvd(s)
divmod(x,y)
submod(x, v)
dsm( x, w )
chsvd(s)
}
\arguments{
  \item{m,n,num,den}{Integer}
  \item{a,b,c,prec,x,y}{Real}
  \item{r}{Real vactor}
  \item{rel}{Boolean}
  \item{s}{square matrix, result of \code{\link{svd}} }
  \item{v}{real vector > 0, preferably cumsum of some other positive vector}
  \item{w}{real vector > 0}
}
\details{
\code{mod} = m\%\%n\cr
\code{modS} : modulo symmetric to 0.\cr
\code{modR}: modulo for reals, towards negative infinity.\cr
\code{zero} returns 0.0 on any argument\cr
\code{one} returns 1.0 on any argument\cr
\code{onebyx} = 1.0/x\cr
\code{sqr} = x^2\cr
\code{powr} = a^x, with  0^0 := 1, 0^x := 0\cr
\code{equal} x == y\cr
\code{equalFuzzy} One can choose between relative and absolute precision\cr
\code{safeDiv} Compute quotient, set 0/0 -> 1,  and safeguard r/0 <- c3Q otherwise  \cr
\code{quotmean} Compute quotient of means of non-NA elements of x by y\cr
\code{solveQeq} Solve the quadratic equation given by the coefficients, return two solutions if a != 0, else one solution, possibly NA\cr
\code{inrange} Check if x is in the range r\cr
\code{chsvd} Check for \code{\link{svd}} to reproduce matrix \cr
\code{divmod} div, mod combined for ease of use  \cr
\code{submod} analog to divmod for unequally spaced data  \cr
\code{dsm} combination of divmod and submod, used in \code{\link{Date}}
}
\examples{
mod((-3:5),4 ) # 1 2 3 0 1 2 3 0 1
modS((-3:5),4) # -3 -2 -1  0  1  2  3  0  1
x <- 200; y <- x + 0.1
equalFuzzy(x,y,0.1*c(10^(-3:0))) # FALSE  TRUE  TRUE  TRUE
equalFuzzy(x,y,0.1*c(10^(-3:0)),FALSE) # FALSE FALSE FALSE  TRUE
safeDiv(0:3,c(0,0:2)) # 1.552518e+231
solveQeq(0,0,1) # NA NA
solveQeq(0,1,0) # 0
solveQeq(0,1,1) # -1
solveQeq(1,0,0) #  0 0
solveQeq(1,0,1) # 0-1i 0+1i
solveQeq(1,1,0) #  -1  0
solveQeq(1,1,1) #  -0.5-0.866025i  -0.5+0.866025i
solveQeq(sample(1:4,1),sample(1:4,1),sample(1:4,1))
  x <- matrix(rnorm(9),3,3)
  s <- svd(x)
  lV(s$d)
  norm(chsvd(s) - x) # 9.4368957e-16
submod(8.1,c(10.3, 31) )  # 0.0 8.1
submod(18.1,c(10.3, 31) )  # 1.0 7.8
}
\note{ see also examples of \code{\link{date}}
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
