\name{panel}
\alias{panel.cor}
\alias{panel.hist}
\title{Alternative panel functions for lattice plots}
\description{
  Functions which can be used instead of the default functions in panel
	plots. 
}
\usage{
panel.hist(x, ...)
panel.cor(x, y, digits=2, prefix="", cex.cor)
}
\arguments{
  \item{x, y}{variables defining the contents of the panel.}
  \item{digits}{Number of decimals after dot with which correlations will be printed.}
  \item{prefix}{Prefix text for numbers.}
  \item{cex.cor}{Determines height of printed digits, may be missing.}
  \item{\dots}{graphical parameters can be supplied. see function definition for details.}
}
\examples{
n <- 1000; a <- rnorm(n,mean=1)
x <- matrix(c(a,a+2*log(runif(n)),a^2+0.2*rnorm(n,mean=1)),nrow = n)
pairs(x,lower.panel=panel.smooth, diag.panel=panel.hist,
upper.panel=panel.cor, labels = c("rnorm","rnorm+log(runif)","rnorm^2"))
}
\author{?? \email{}}
\keyword{device}
\keyword{hplot}
