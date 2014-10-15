\name{Const}
\alias{cMAXREALBY38}
\alias{cMAXREALBY3Q}
\alias{GreatestIntAsRealF}
\alias{ASCII}
\alias{HexDig}
\alias{spacC}
\title{
Constants
}
\description{
Constants and determination of the greatest integer K which is distiguishable from (K+1), both represented as real
}
\details{
   \code{cMAXREALBY3Q} = .Machine$double.xmax^0.75\cr
  \code{cMAXREALBY38} = sqrt(cMAXREALBY3Q) \cr
  \code{ASCII} = ASCII characters corresponding to  (0), 1..256 \cr
  \code{HexDig} = '1' - '9', 'A' - 'F' \cr
  \code{spacC} =  "  ", is extra space in indexLine and charMat\cr
}
\author{  Christian W. Hoffmann <christian@echoffmann.ch>}
\keyword{arith}
