\name{shapiro.wilk.test}
\alias{shapiro.wilk.test}
\title{Shapiro-Wilk Normality Test}
\description{
  Performs the Shapiro-Wilk test for normality. 
}
\usage{
  shapiro.wilk.test(x)
}
\arguments{
  \item{x}{a numeric vector of data values, the number of which must be between 3 and 5000. Missing values
   are allowed.}
}
\value{
  A list containing the following components:
  \item{W}{ the value of the Shapiro-Wilk statistic.}
  \item{n}{ length(x)}
  \item{p}{ the p-value for the test.}
}
\author{??}
\seealso{ \code{\link{shapiro.test}} }
\examples{
  shapiro.wilk.test(rnorm(100, mean = 5, sd = 3))  # $p 0.169547
  shapiro.wilk.test(runif(100, min = 2, max = 4))  # $p 6.09393e-06
}
\keyword{htest}
