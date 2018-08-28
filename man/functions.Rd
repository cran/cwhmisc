\name{functions}
\alias{Cayley}
\alias{chsvd}
\alias{divmod}
\alias{divmodL}
\alias{dsm}
\alias{equal}
\alias{equalFuzzy}
\alias{exch}
\alias{frac}
\alias{int}
\alias{inrange}
\alias{Km}
\alias{Ko}
\alias{last}
\alias{LE}
\alias{loop.vp}
\alias{LS}
\alias{lV}
\alias{mod}
\alias{modR}
\alias{modS}
\alias{norm2}
\alias{one}
\alias{onebyx}
\alias{powr}
\alias{pythag}
\alias{quotmean}
\alias{safeDiv}
\alias{signp}
\alias{solveQeq}
\alias{sqrtH}
\alias{sqr}
\alias{submod}
\alias{zero}
\title{Functions for testing and other}
\description{
 Functions for testing on equality, exactly or with a tolerance, functions usable as parameters in other functions, pythagorean sums, etc.
}
\usage{
  chsvd( s )
  chsvd( s )
  divmod( i, n )
  divmodL( i, n )
  dsm( x, w )
  equal( x, y )
  equalFuzzy( x, y, prec=8*.Machine$double.eps, rel=TRUE )
  exch( x, L, R )
  frac(x,d)
  int( x )
  inrange( x, y )
  Ko(z)
  Km(z)
  last( x )
  LE ( x )
  loop.vp( ve, overlap=1 )
  LS ( )
  lV ( x )
  mod( x, y )
  modR( x, y )
  modS( x, y )
  norm2( x )
  one ( x )
  onebyx( x )
  powr( a, x )
  pythag( a, b )
  quotmean( x, y )
  safeDiv( num, den )
  signp( x )
  solveQeq( a, b, c )
  sqr( x )
  sqrtH( x )
  submod( x, v )
  zero ( x )
}
\arguments{
  \item{prec,L,R}{Real}
  \item{a,b,c,z}{Complex}
  \item{i}{Integer vector}
  \item{d}{If not missing, 'frac' shows 'd' decimals after "." as integer }
  \item{n,num,den}{Integer}
  \item{rel}{Boolean}
  \item{s}{square matrix, result of \code{\link{svd}} }
  \item{v}{real vector > 0, preferably cumsum of some other positive vector}  \item{ve}{real any vector or matrix}
  \item{overlap}{integer vector, giving element indices/column numbers to be appended at the end, see examples}
  \item{x,y}{Real vector}
  \item{w}{Real vector > 0}
}
\value{
  \code{exch}: Exchanges elements 'L' and 'R': x[which( x == L )] <- R; x[which( x == R )] <- L\cr
  \code{K}: Cayley transform \code{ (z - i)/(z + i)}\cr
  \code{Km}: \code{ (1 + z)i/(1 - z)}, inverse transformation of \code{K}
  \code{norm2}: 2-norm.\cr
  \code{last}: last element of vector
  \code{LE}: = length of vector
  \code{pythag}: c(A,B,C), A=final a' = sqrt( a^2 + b^2 ) without squaring and \cr
  taking the square root, avoiding overflow and underflow, B=final b',\cr
           C=residual = final (b'/a')^2, see note. \cr
  \code{signp}: ifelse( is.na(x) | (!is.finite(x) | x>=0),1,-1 ), avoiding\cr
            NA, NaN and 0 in the result.\cr
  \code{sqrtH}: Square root with Halley's hyperbolical method.
}
\details{
  BEWARE of NAs !!\cr
  \code{chsvd} Check for \code{\link{svd}} to reproduce matrix. \cr
  \code{divmod} rbind(div, mod) for ease of use.\cr
  \code{divmodL} list(d = div, m = mod) \cr
  \code{dsm} combination of divmod and submod, used in \code{\link{Jul2Dat}}  \cr
  \code{equalFuzzy} One can choose between relative and absolute precision\cr
  \code{equal} x == y, of same length.\cr
  \code{inrange} Check if 'x' (scalar) is in the range (min(y),max(y)).\cr
  \code{int} returns 'x' as integer in fix format\cr
  \code{last} return the last element of a vector.\cr
  \code{LE} short for 'length(x)'.\cr
  \code{LS} short for '.Last.value'.\cr
  \code{loop.vr}: loop around vector with overlap.\cr
  \code{LS} short for '.Last.value'.\cr
  \code{modS}: same as 'mod', symmetric to 0.\cr
  \code{mod} = x \%\% y, x and y with same number of elements. \cr
  \code{onebyx} = 1.0/x\cr
  \code{one} returns 1.0, same length as 'x'\cr
  \code{powr} = x^y, with  0^0 := 1, 0^y := 0, any y\cr
  \code{quotmean} Compute quotient of means of non-NA elements of x by y\cr
  \code{safeDiv} Compute quotient, set 0/0 -> 1,  and safeguard r/0 <- \code{\link{c3Q}} otherwise  \cr
  \code{signp(0)} -> 1, \code{signp(complex)} -> NA ! \cr
  \code{solveQeq} Solve the quadratic equation \code{a*z^2+ b*z+c} given\cr
  the coefficients \code{a,b,c} OR \code{c(a,b,c)}, returns always *two*\cr
  solutions; if \code{a = b = 0} returns {c(Inf, Inf)}\cr
  \code{sqr} = x^2\cr
  \code{submod} analog to divmod for unequally spaced data, c(greatest index \code{gi} of \code{v} s.t. v < x, x - v['gi']  \cr
  \code{zero} returns 0.0, same length as 'x'\cr
}
\examples{
int (c(0,pi,2*pi,30*pi))    # 0  3  6 94
frac(c(0,pi,2*pi,30*pi))    # 0.000000 0.141593 0.283185 0.247780
frac(c(0,pi,2*pi,30*pi), 3) # 0 142 283 248
y <- c( Inf, -Inf,NA,  NaN, -NaN,-1, 0, 1 )
signp(c(-1:1,NA,NaN,Inf, -Inf)) # -1   1   1  1  1     1  1
#  instead of sign() =            -1   0   1  NA NaN   1  -1
mod((-3:5),4 ) # 1 2 3 0 1 2 3 0 1
modS((-3:5),4) # -3 -2 -1  0  1  2  3  0  1
x <- 200; y <- x + 0.1
equalFuzzy(x,y,0.1*c(10^(-3:0))) # FALSE  TRUE  TRUE  TRUE
equalFuzzy(x,y,0.1*c(10^(-3:0)),FALSE) # FALSE FALSE FALSE  TRUE
loop.vp(1:4) #  1 2 3 4 1
loop.vp(matrix(1:12,nrow=3),c(2,4))
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    1    4    7   10    4   10
#[2,]    2    5    8   11    5   11
#[3,]    3    6    9   12    6   12
safeDiv(0:3,c(0,0:2)) # 1.552518e+231
signp(c(-1:1,NA,NaN,Inf, -Inf)) # -1   1   1  1  1     1  1
#  instead of sign() =            -1   0   1  NA NaN   1  -1
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
exch(LETTERS, "A", "Y") # "Y" "B" ... "W" "X" "A" "Z"
exch(1:5, "2", "Y") # "1" "Y" "3" "4" "5"
pythag(19,180) # 1.8100000e+02 3.8414499e-23
}
\note{
see also examples of \code{\link{date}}\cr
Note that \code{ 1 } results with \code{ signp( 0 ) };\cr
  It is \emph{not} possible to discriminate between \code{Inf} and \code{-Inf}, by definition in R,\cr
  \emph{but}: as.character(-Inf) = "-Inf".
\code{pythag}: The invariant of the iteration is sqrt(a^2 + b^2), iterating a':=max(a,b) and reducing b':=min(a,b).
}
\references{
  Moler, C. and Morrison. D, 1983 \emph{Replacing Square Roots by Pythagorean Sums}, IBM J.Res.Devel., \bold{27}, 6, 577--589.\cr
\href{http://www.jjam.de/JavaScript/Mathematik/Primzahlen.html}{jjam(Prime numbers)};  \href{http://www.mathpath.org/Algor/squareroot/algor.square.root.halley.htm}{Mathpath(Formula for Halley)};  \href{https://en.wikipedia.org/wiki/Halley's_method}{Wikipedia(Derivation of Halley)}
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{arith}
