\name{FinneyCorr}
\alias{FinneyCorr}
\alias{FC.lm}
\alias{R2.lm}
\alias{s.lm}
\title{Finney's correction to log normally distributed data, r-squared
  and standard deviation of a linear model.}
\description{
  FinneyCorr: Finney's correction factor K in \eqn{x = e^{\ln x} * K} (see \code{Note}), to be used if \eqn{\ln x} is normally distributed with standard deviation \eqn{s_{\ln x}}.
	FC.lm, R2.lm, s.lm:  Finney's correction, \eqn{R^2} and standard deviation extracted from an object of class "lm".
}
\usage{
  FinneyCorr(s,n)
  FC.lm(lmobj)
  R2.lm(lmobj)
  s.lm(lmobj)
}
\arguments{
  \item{s}{Standard deviation of log data.}
  \item{n}{Number of data points.}
  \item{lmobj}{An object of class "lm".}
}
\note{
  \eqn{K := e^{s_{\ln}^2/2} \,
	\left\{1-\frac{s_{\ln}^2}{4n}(s_{\ln}^2+2)+\frac{s_{\ln}^4}{96n^2}(3s_{\ln}^4+44s_{\ln}^2+84) \right\}}
}
\examples{
FinneyCorr(0.346274,24+3)  #  1.059306936

ok <- RNGkind()
RNGkind(kind = "default", normal.kind = "default")
set.seed(2009, kind = "default")
x <- rnorm(1000); y <- 0.1*rnorm(1000)
## Reset:
RNGkind(ok[1])

lmo <- lm(y ~ x)
FC.lm(lmo)  # 1.00472
R2.lm(lmo)  # 6.1926e-05
s.lm(lmo)   # 0.0970954
}
\references{Finney D.J., 1941. On the distribution of a variable whose
  logarithm is normally distributed. J. R. Stat. Soc., B 7: 155-161
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{models}
\keyword{regression}

