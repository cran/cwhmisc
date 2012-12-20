\name{num2Latex}
\alias{num2Latex}
\title{Convert numeric containing e+-power}
\description{
  Latex string with power notation
}
\usage{
  num2Latex(x, digits = 0)
}
\arguments{
  \item{x}{numerical vector}
  \item{digits}{digits to show, see also \code{\link{options}} \code{scipen}
  }
}
\value{
  Vector of strings representing the given numbers, xe-y -> $x . 10 ^{-y}$
}
\author{
\email{dimitris.rizopoulos@med.kuleuven.be}
}
\examples{
 z <- c(1.5, 5e-12, 2.33e-03, 8.12e+10, 2)
 num2Latex(z)     #  1.5,  5 \cdot 10^{-12},  0.00233,  8.12 \cdot 10^{10},  2
 num2Latex(z, 2)  #  1.5,  5 \cdot 10^{-12},  2.33 \cdot 10^{-3},  8.12 \cdot 10^{10},  2 
 num2Latex(z, -3) #  1.5,  5 \cdot 10^{-12},  0.00233, 81200000000, 2 
}
\keyword{print}
