\name{frac}
\alias{contfrac}
\alias{evalcfr}
\alias{toCFrac}
\alias{toCFrac2}
\title{Fractional part of number, continuous fractions}
\description{
  Split off fractional part of a number, compute and evaluate continuous fractions.
}
\usage{
  contfrac( x, depth = 13, f=floor ) 
  evalcfr( cf )
  toCFrac( x, depth=5)
  toCFrac2( x, depth=5)
}
\arguments{
  \item{x}{Real}
  \item{f}{function to use, normally 'floor', otherwise 'round' or 'trunc'}
  \item{cf}{Vector of integers representing the continued fraction of a real number}
  \item{depth}{Integer}
}
\value{
  \code{int} integer part truncate towards 0.\cr
  \code{frac} fractional part, if \code{d} is missing; else \cr
     \eqn{round(10^d*fractional part)}, i.e. the fractional part as "integer" (rounded).\cr
  \code{contfrac} Convert to simple continued fraction representation, \eqn{cf := a_1 + 1/(a_2 + 1/(a_3 ... ))}.\cr
  
  \code{evalcfr} Evaluate simple continued fraction to corresponding real.\cr
  \code{toCFrac} Build rational approximation \code{num/den} to \code{x} using forward continued fraction recursion to a depth of \code{depth}. Stopping criterion: either \code{depth} is reached, or abs(x - num/den) is increasing again.\cr
  \code{toCFrac2} same as \code{toCFrac}, but vectors of partial numerators and denominators are returned.
}
\note{\code{d} not missing is practical for use in \code{\link{dc}}
\cr
  For \code{confrac} see also \code{link[MASS]{fractions}}.
  from Mathematics 5335 Fall 2009
  The Euclidean Algorithm and Continued Fractions
}
\examples{
(pcf <- contfrac(pi)) # 3, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, (1)
## last integer incorrect due to rounding errors
evalcfr(pcf)-pi # 0
## To see the first approximants of pi, all of them famous:
for(ii in 1:15) {x<-toCFrac(pi,ii)
print(paste(ii,":",x$num,"/",x$den,"="))
print(paste(formatFix(x$num/x$den,15),", error = ",x$num/x$den-pi))}
# Note how the approximations taper off after depth 5:
# 10 3959189 / 1260249 =  3.141592653515298 -7.44955208631382e-11"
## Same, all at once:
F <- toCFrac2(pi,5)  # $num  3  22 333 355  $den  1   7 106 113
toCFrac( pi, 10 )  #  
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{arith}
