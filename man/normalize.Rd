\name{normalize}
\alias{normalize}
\alias{normalize1}
\alias{Nd}
\alias{sigplaces}
\alias{checkNormalize}
\title{
base power and multiplier, significant places
}
\description{ Split a number into base power and multiplier so that x =
  a*base^e, with abs( a ) in [1, base); check normalization; compute
  number of significant places
}
\usage{
  normalize( x, base=2 )
  checkNormalize( no )
  Nd(x, base=10)
  sigplaces(x, base=10, rnd=0)
  checkNormalize( no )
}
\arguments{
  \item{x}{Real vector}
  \item{base}{Base of power}
  \item{no}{result of \code{normalize} }
  \item{rnd}{Integer >0 / <0, rounding to r digits after/before "." }
}
\value{
  \code{normalize:} data-frame with one column c(a,e,base) for each \code{x}, such that \emph{x  = a*base^e}, abs( a ) in \emph{ [1, base)}, but \emph{a:=x, e:=0} for \emph{ x=0, NA, +-Inf}.\cr
  \code{normalize1:} as \code{normalize:}, but abs( a ) in \emph{[1/base, 1)}\cr
  \code{Nd:} log to base \code{base}, \emph{ 1 for x=0 }.\cr
  \code{sigplaces:} number of places necessary for printing trunc(x); c(2,3,4,3) for c(NA,Inf,-Inf,NaN).
  \code{checkNormalize:} reconvert argument to number. 
}
\details{
\code{normalize(c(+-Inf, NA))} will result in \code{c(+-Inf,NA,1).}
}
\examples{
(xx <- c(exp(1),pi,NA, Inf, -Inf,10,100,c(1,10)*exp(1)) )
(x2 <- normalize(xx,2))
#          A         B  C   D   E    F      G         H         I
# a 1.3591409 1.5707963 NA Inf Inf 1.25 1.5625 1.3591409 1.6989261
# e 1.0000000 1.0000000  0   0   0 3.00 6.0000 1.0000000 4.0000000
# b 2.0000000 2.0000000  2   2   2 2.00 2.0000 2.0000000 2.0000000

(x32 <- normalize1(xx,2))
#           A          B   C   D   E     F       G          H          I
# a 0.67957046 0.785398 NA Inf Inf 0.625 0.78125 0.67957 0.849463
# e 2.00000000 2.000000  1   1   1 4.000 7.00000 2.00000 5.000000
# b 2.00000000 2.000000  2   2   2 2.000 2.00000 2.00000 2.000000

(x10 <- normalize(xx,10))
#           A          B  C   D   E  F  G          H          I
# a  2.7182818  3.1415927 NA Inf Inf  1  1  2.7182818  2.7182818
# e  0.0000000  0.0000000  0   0   0  1  2  0.0000000  1.0000000
# b 10.0000000 10.0000000 10  10  10 10 10 10.0000000 10.0000000

(x7 <- normalize(xx,7))
#           A         B  C   D   E         F         G         H         I
# a 2.7182818 3.1415927 NA Inf Inf 1.42857 2.0408 2.71828 3.8832598
# e 0.0000000 0.0000000  0   0   0 1.00000 2.0000 0.00000 1.0000000
# b 7.0000000 7.0000000  7   7   7 7.00000 7.0000 7.00000 7.0000000

sigplaces(-9.999) #
sigplaces(pi/100) #

all.equal(checkNormalize(x2), checkNormalize(x7)) # TRUE

}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{math}
