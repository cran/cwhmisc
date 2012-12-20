\name{Const}
\alias{cLOCKWISE}
\alias{cE}
\alias{cLOG2E}
\alias{cLOG10E}
\alias{cLN2}
\alias{cLN10}
\alias{c2PI}
\alias{cPIBY2}
\alias{cPIBY4}
\alias{c1BYPI}
\alias{c2BYPI}
\alias{c1BYSQRTPI}
\alias{c2BYSQRTPI}
\alias{cSQRT2}
\alias{cSQRT2BY2}
\alias{cMAXREALBY38}
\alias{cMAXREALBY3Q}
\alias{GreatestIntAsRealF}
\alias{ASCII}
\alias{HexDig}
\alias{spacC}
\alias{PRIMES}

\title{
Constants
}
\description{
 Elementary constants and 'GreatestIntAsRealF': determination of the greatest integer K which is distiguishable from (K+1), both represented as real
}
\details{
  \code{cE}      =  2.718281828459045235     ## e\cr
  \code{cLOG2E}  =  1.442695040888963407     ## log(basis 2)  of e\cr
  \code{cLOG10E} =  0.4342944819032518277    ## log(basis 10)  of e\cr
  \code{cLN2}    =  0.6931471805599453094    ## log(basis e)  of 2\cr
  \code{LN10}   =  2.302585092994045684     ## log(basis e)  of 10\cr
  \code{c2PI}    =  6.283185307179586477     ## 2pi\cr
  \code{cPIBY2}  =  1.570796326794896619     ## pi/2\cr
  \code{cPIBY4}  =  0.7853981633974483096    ## pi/4\cr
  \code{c1BYPI}  =  0.3183098861837906715    ## 1/pi\cr
  \code{c2BYPI}  =  0.6366197723675813431    ## 2/pi\cr
  \code{c1BYSQRTPI} =  0.5641895835477562869    ## 1/sqrt(pi)\cr
  \code{c2BYSQRTPI} =  1.12837917095512574     ## 2/sqrt(pi)\cr
  \code{cSQRT2}	   =  1.414213562373095049    ## sqrt(2)\cr
  \code{cSQRT2BY2}  =  0.7071067811865475244    ## sqrt(2)/2\cr
  \code{cMAXREALBY3Q} = .Machine$double.xmax^0.75\cr
  \code{cMAXREALBY38} = sqrt(cMAXREALBY3Q) \cr
  \code{GreatestIntAsRealF} Greatest integer K which is distiguishable from (K+1), both represented as real\cr
  \code{ASCII} = ASCII characters corresponding to  (0), 1..256 \cr
  \code{HexDig} = '1' - '9', 'A' - 'F' \cr
  \code{spacC} =  "  ", is extra space in indexLine and charMat\cr
  \code{PRIMES} The first 115 primes
}
\author{  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>}
\keyword{arith}