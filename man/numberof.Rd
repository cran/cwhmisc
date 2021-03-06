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
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{distribution}
