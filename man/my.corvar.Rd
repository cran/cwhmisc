\name{my.var}
\alias{my.var}
\alias{my.cor}
\title{ Correlation, Variance and Covariance (Matrices)}
\description{
  \code{my.var} and \code{my.cor} are deprecated,
	use \code{var}, \code{cor} and \code{cov}. 
	For backward compatibility they are kept, but are
 	implemented as calls to the new functions.
}
\usage{
my.var(x, y) -> var(x, y = NULL, na.rm = TRUE, use = "complete")
my.cor(x, y) -> cor(x, y = NULL, use = "complete")
}
\arguments{
  \item{x}{ a numerical vector, matrix or data frame.}
  \item{y}{ a numerical vector of the same length as \code{x} }
}
\value{
  matrix, if \code{x} and \code{y} have more than one column. 
}
\examples{
datavec <- datavec1 <- matrix(rnorm(100),10,10)
datavec1[1,2] <- datavec1[5,7] <- NA
my.var(datavec)
my.var(datavec1)
\testonly{## regression test for PR#376}
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{ multivariate }
