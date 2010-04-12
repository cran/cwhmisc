\name{frac}
\alias{frac}
\title{Fractional part of number}
\description{
  Split off fractional part of a number
}
\usage{
  frac(x,d)
}
\arguments{
  \item{x}{Numerical vector.}
  \item{d}{If not missing, determines number of decimals after "."}
}
\value{
  fractional part, if \code{d} is missing; else
  \eqn{round(10^d*fractional part)}, i.e. the digits without the leading "0.".
}
\note{\code{d} not missing is practical for use in \code{\link{dc}}
}
\examples{
frac(c(0,pi,2*pi,30*pi))    # 0.000000 0.141593 0.283185 0.247780
frac(c(0,pi,2*pi,30*pi),3)  # 0 142 283 248
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{arith}
