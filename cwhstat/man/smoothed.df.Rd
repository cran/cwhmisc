\name{smoothed.df}
\alias{smoothed.df}
\title{Fit cumulative distribution from kernel estimate.}
\description{
Given a kernel density estimate, this function carries out a (very quick
and dirty) numerical integration, and then fits a spline to get a function which can be used to look up cumulative probabilities.
}
\usage{
smoothed.df(d)
}
\arguments{
  \item{d}{kernel density estimate}
}
\value{
  The spline function approximating the df.
}
\author{Ross Ihaka, \email{ihaka@stat.auckland.ac.nz}}
\examples{
x <- rnorm(1000) + ifelse(runif(1000) > .5, -3, 3)
d <- density(x)
F <- smoothed.df(d)   # F returns cumulative probs

# Plot the true (red) and estimated (blue) density functions
par(mfrow=c(1,2))
curve(0.5 * dnorm(x, -3) + 0.5 * dnorm(x, 3), -7, 7, col="red")
lines(d, col="blue")

# Plot the true (red) and estimated (blue) distribution functions
curve(0.5 * pnorm(x, -3) + 0.5 * pnorm(x, 3), -7, 7, col="red")
curve(F(x), add=TRUE, col="blue")
}
\keyword{distribution}
