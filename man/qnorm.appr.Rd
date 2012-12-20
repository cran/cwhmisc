\name{qnorm.appr}
\alias{qnorm.ap16}
\alias{qnorm.app4}
\alias{qnorm.app3}
\title{Approximation to the inverse normal distribution function.}
\description{
  \code{qnorm.ap*} approximate the normal quantile function. They
	compute \code{x} such that P(x) = Prob(X <= x) = \code{p}. 
}
\usage{
  qnorm.app3(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  qnorm.app4(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  qnorm.ap16(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
}
\arguments{
  \item{p}{vector of probabilities.}
  \item{mean}{vector of means.}
  \item{sd}{vector of standard deviations.}
  \item{log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P(X <= x), otherwise, P(X > x).}
}
\value{
  \code{qnorm.ap*} gives the quantile function for the different approximations.
}
\note{
  \code{qnorm.ap16} is the approximation used in \code{\link{qnorm}}. The others have an absolute error < \eqn{10^{-3}} and \eqn{10^{-4}}.
}
\section{Warning}{
	If \code{p <= 0} or \code{p >= 1}, then NA will be returned.
	
  If \code{p} is very close to 1, a serious loss of significance may be
	incurred in forming \code{c := 1 - p}, resulting in \code{p = 0}. In
	this case \code{c} should be derived, if possible, directly (i.e. not
	by subtracting \code{p} from 1) and evaluate
	\code{qnorm(p,\dots,lower.tail=B)} as \code{qnorm(c,\dots,lower.tail = (B==FALSE))}.  
}
\examples{
  prec <- function(x,y,z=y) max(abs((x-y)/z)) # relative precision
  x2 <- -0.6744897501960817; p2 <- 0.25
  x0 <- -3.090232306167814;  p0 <- 0.001
  xm <- -9.262340089798408;  pm <- 1.0e-20
  x  <- c((-100:0)/10,x2,x0,xm)
  p  <- pnorm(x)
  x3 <- qnorm.app3(p)
  x4 <- qnorm.app4(p)
  x1 <- qnorm.ap16(p)
# Check relative precision of approximations
  prec(x,x3,1)  # 0.002817442
  prec(x,x4,1)  # 0.0004435874
  prec(x,x1,1)  # 0.1571311 why so bad ?
  prec(x,qnorm(p),1)  # 1.776357e-15
# Special values
  prec(qnorm.app3(p2),x2)  # 0.004089976
  prec(qnorm.app3(p0),x0)  # 0.0007736497
  prec(qnorm.app3(pm),xm)  # 7.29796e-06
  prec(qnorm.app4(p2),x2)  # 0.0004456853
  prec(qnorm.app4(p0),x0)  # 9.381806e-05
  prec(qnorm.app4(pm),xm)  # 4.151165e-05
  prec(qnorm.ap16(p2),x2)  # 0
  prec(qnorm.ap16(p0),x0)  # 2.874148e-16
  prec(qnorm.ap16(pm),xm)  # 0.01211545
  
}
\source{
	\code{qnorm.app3} and \code{qnorm.app4}: Abramowitz and Segun, Dover,
	1968, formulae 26.2.22 and 26.2.23, 

	\code{qnorm.ap16}: Wichura, M. J., 1988. Algorithm AS 241: The Percentage Points of the
	Normal Distribution. Applied Statistics 37, 477-484.
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> 
}
\keyword{distribution}
