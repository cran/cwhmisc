\name{dc}
\alias{dc}
\alias{dcn}
\title{Convert number for use in decimal dot centered table columns}
\description{Replace "." in a number by "&" for LaTeX tables using
  column specification r\@\{.\}l
}
\usage{
  dc (x,d,ch="&")
  dcn(x,d,ch="&")
}
\arguments{
  \item{x}{Numerical vector.}
  \item{d}{If not missing, determines number of decimals after ".". d >=
	0}
  \item{ch}{Substitute "." by ch}
}
\value{
  string representation of \code{x} suitable for table column centered
  on "."
}
\note{\code{dc} and \code{dcn} produce identical results for \code{d >
	0}, but not for \code{ d <= 0 }, see \code{\examples}.
}
\section{Warning}{\code{d} missing is an error. \code{dcn(x,0)} does not serve its
	intended purpose, see examples!
}
\examples{
  dc (c(0,pi,2*pi,-30*pi),3)   # "0&0"     "3&142"   "6&283"   "-94&248"
  dcn(c(0,pi,2*pi,-30*pi),3)   # "0&000"   "3&142"   "6&283"   "-94&248"
#  d = 0:
  dc (c(0,pi,2*pi,-30*pi),0)   # "0&0"  "3&0"  "6&0"  "-94&0"
  dcn(c(0,pi,2*pi,-30*pi),0)   # "0"    "3"    "6"    "-94",  beware !
#  d < 0:
  dc (c(0,pi,2*pi,-30*pi),-3)  # "0&0"    "3&0"    "6&0"    "-94&0"
  dcn(c(0,pi,2*pi,-30*pi),-3)  # "0&000"  "3&142"  "6&283"  "-94&248"
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{arith}
