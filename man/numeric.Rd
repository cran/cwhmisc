\name{numeric}
\alias{scm}
\alias{EulerPhi}
\alias{gcd}
\alias{EuclidExtended}
\alias{modexp}
\title{Number theoretic functions}
\description{
Simple number theoretic functions
}
\usage{
scm( m, n )
EulerPhi( n )
gcd( m, n )
EuclidExtended( m, n )
modexp( a, b, n )
}
\arguments{
  \item{a,b,m,n}{Integer}
}
\details{
  \code{EulerPhi} Eulers totient function = #divisors of n.
  \code{scm, gcd} Smallest common multiple, Greatest common divisor.
  \code{EuclidExtended} Computes a, b which solve the equation a*m + b*n = gcd(m,n).
  \code{modexp} Exponentiation modulo an integer.
}
\examples{
 scm(35,133) # 665
 gcd(35,133) # 7
 EuclidExtended(35,133) #   4 -1, meaning 4*35 -1*133 = 7
 EulerPhi(60) # 16
 modexp(3,10,7)  # 4
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{character,arith}