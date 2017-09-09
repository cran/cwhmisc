\name{Const}
\alias{c38}
\alias{c3Q}
\alias{GreatestIntAsRealF}
\alias{ASCII}
\alias{HexagesDig}
\alias{HexDig}
\alias{EXPCHAR}
\alias{tau}
\title{
Constants
}
\description{
Constants
}
\details{
  \code{c3Q} := .Machine$double.xmax^0.75, used for computations below Inf, also\cr
  \code{c38} := sqrt(c3Q) \cr
  \code{ASCII} := ASCII characters corresponding to  (0), 1..256 \cr
  \code{HexDig} := '1' - '9', 'A' - 'F', 'a' - 'f' \cr
  \code{HexagesDig} := '1' - '9', 'A' - 'Y', 'a' - 'y' \cr
  \code{EXPCHAR} := "z", exponential marker used for bases other then 10 (for base 10 "e" is used as usual);
  \code{TAU} := (1+sqrt(5))/2 # golden section constant = 1.6180
}
\seealso{\code{\link{r2Be}}
}
\author{  Christian W. Hoffmann <christian@echoffmann.ch>}
\keyword{arith}
