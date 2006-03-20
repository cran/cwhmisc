\name{summaryFs}
\alias{summaryFs}
\title{Print extended summary of lm.}
\description{
  Print summary of lm, std.dev and Finney's correction to log normally distributed data.
}
\usage{
  summaryFs(lm)
}
\arguments{
  \item{lm}{Result of an lm(log(y) ~ .)}
}
\examples{
  x <- rnorm(1000); y <- 0.1*rnorm(1000)
  summaryFs(lm(y ~ x))
}
\note{ Uses \code{\link{FC.lm}} and \code{\link{s.lm}}
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{models}
\keyword{regression}

