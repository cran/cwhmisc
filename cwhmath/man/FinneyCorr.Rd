\name{FinneyCorr}
\alias{FinneyCorr}
\alias{FC.lm}
\alias{s.lm}
\title{Finney's correction to log normally distributed data.}
\description{
  FinneyCorr: Finney's correction factor K in \eqn{x = e^{ln x} * K}, with \newline
  \eqn{K = e^{s_{\ln}^2/2} \,
    \left\{ 1-\frac{s_{\ln}^2}{4n}(s_{\ln}^2+2)+\frac{s_{\ln}^4}{96n^2}(3s_{\ln}^4+44s_{\ln}^2+84)
      \right\} }{}, to be used if \eqn{ln x} is normally distributed with standard deviation \eqn{s_{ln x}}.
	FC.lm, s.lm:  Finney's correction and standard deviation extracted from an object of class "lm".
}
\usage{
  FinneyCorr(s,n)
  FC.lm(lmobj)
  s.lm(lmobj)
}
\arguments{
  \item{s}{Standard deviation of log data.}
  \item{n}{Number of data points.}
  \item{lmobj}{An object of class "lm".}
}
\examples{
FinneyCorr(0.346274,24+3)  #>  1.059306936
x <- rnorm(100); y <- rnorm(100)
lmo <- lm(y ~x)
FC.lm(lmo)
s.lm(lmo)
}
\references{Finney D.J., 1941. On the distribution of a variable whose logarithm is normally distributed. J. R. Stat. Soc., B 7: 155-161}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{models}
\keyword{regression}

