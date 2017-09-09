\name{invgauss}
\alias{dinvgauss}
\alias{pinvgauss}
\alias{qinvgauss}
\alias{rinvgauss}
\title{Inverse Gaussian Distribution}
\description{
  Density, cumulative probability, quantiles and random generation for the inverse Gaussian distribution.
}
\usage{
  dinvgauss(x, mu = stop("no shape arg"), lambda = 1)
  pinvgauss(q, mu = stop("no shape arg"), lambda = 1)
  rinvgauss(n, mu = stop("no shape arg"), lambda = 1)
}
\arguments{
  \item{n}{Integer}
  \item{q, x}{ Real}
  \item{mu,lambda}{positve array of integers, means and scaling parameter}
}
\value{
  \code{dinvgauss:} Inverse Gaussian distribution function \cr
  \code{pinvgauss:} Random variates from inverse Gaussian distribution \cr
  \code{rinvgauss:} Quantiles of the inverse Gaussian distribution \cr
}
\examples{
  n <- 10; 
}
\note{
\deqn{p(x;\mu,\lambda) = \sqrt{\frac{\lambda}{2\pi x^3}}e^{\frac{-\lambda(x-\mu)^2}{2x\mu^2}}}{%
  p(x) = sqrt(lambda/(2*pi*x^3))e^{-lambda(x-mu)^2/(2*x mu^2)}}
}
\author{Gordon Smyth, \email{gks@maths.uq.edu.au}, from sources of <paul.bagshaw@cnet.francetelecom.fr> e.a.}
\references{
 Chhikara and Folks, The Inverse Gaussian Distribution, Marcel Dekker, 1989.
 \url{http://en.wikipedia.org/wiki/Inverse_Gaussian_distribution}
}
\keyword{distribution}
