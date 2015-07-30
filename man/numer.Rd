\name{numer}
\alias{EulerPhi}
\alias{gcd}
\alias{Euclid}
\alias{scm}
\alias{Inv}
\alias{modexp}
\title{Number theoretic functions}
\description{
Simple number theoretic functions
}
\usage{
scm( m, n )
EulerPhi( n )
gcd( a, b )
Euclid( a, b )
Inv(a, n)
modexp( a, b, n )
}
\arguments{
  \item{a,b,m,n}{Integer}
}
\value{ 
  \code{EulerPhi} Eulers totient function = number of divisors of n.
  \code{scm, gcd} Smallest common multiple, Greatest common divisor.
  \code{Euclid} Computes x, y from a, b such that the equation a*x + b*y = gcd(m,n) is satisfied.
  \code{Inv} Modular inverse in a finite ring, NA if not exists.
  \code{modexp} Exponentiation modulo an integer via binary decomposition of exponent.
}
\examples{
 scm(35,133) # 665
 gcd(35,133) # 7
 Euclid(35,133) #   4 -1 7, meaning 4*35 +(-1)*133 = 7
 EulerPhi(60) # 16
 modexp(3,10,7)  # 4
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
