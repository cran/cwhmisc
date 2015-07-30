\name{factor}
\alias{primes}
\alias{Eratosthenes}
\alias{factorN}
\alias{prodN}
\alias{is.prime}
\alias{PRIMES}
\title{
Create primes, factor an integer, combine factors, check if prime
}
\description{
Create primes, determine the prime factors of an integer
  together with their multiplicities, recombine factors, primitive version of the sieve of Eratosthenes.
}
\usage{
primes(n)
Eratosthenes(n)
factorN(n)
prodN(fp)
is.prime(n)
}
\arguments{
\item{n}{positive integer, number of primes, number to be factored, to be tested}
\item{fp}{2-colummn matrix with prime factors and multiplicities}
}
\value{
\item{primes}{Generate the first n primes, also found in PRIMES.}
\item{Eratosthenes}{Execute the sieve of Eratosthenes.}
\item{factorN}{Determine the prime factors together with their multiplicities.}
\item{prodN}{Recombine factors, inverse of factorN.}
\item{is.prime}{Check if positive integer is prime.}
\item{PRIMES}{The first primes up to 17389.}
}
\examples{
(p <- factorN( 423))
## [1,]   3 47
## [2,]   2  1
# meaning 423 = 3^2 * 47^1
prodN(p) # 423
is.prime(.Machine$integer.max) # TRUE
is.prime(16) # FALSE
## check speed of your machine
  s <- Sys.time(); p<-primes(10^4);difftime(Sys.time(),s) 
## Time difference of 2.906242 secs  on my machine  
x <-factorN(.Machine$integer.max)
}
\author{
  Christian W. Hoffmann <hristian@echoffmann.ch>
}
\keyword{math}
