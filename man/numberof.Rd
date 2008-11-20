\name{numberof}
\alias{numberof}
\title{Count the number elements that satisfy a condition.}
\description{
  \code{numberof} counts the number elements that satisfy a condition.
}
\usage{
numberof(x, f)
}
\arguments{
  \item{x}{Numerical array.}
  \item{f}{Logical function emulating the condition to be satisfied.}
}
\examples{
  numberof(c(1:100,NA,NA,NaN),function(x) !is.na(x))
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{distribution}
