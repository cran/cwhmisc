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
  \item{d}{Number of decimals after ".". d >= 1, will be forced internally.}
  \item{ch}{Substitute "." by \code{ch}}
}
\value{
  string representation of \code{x} suitable for table column centered
  on "."
}
\note{
\code{dc} = \code{dcn}, except for \code{ x = integer }.

\code{dc} uses \code{\link[cwhmath]{frac}}, \code{dcn} uses \code{\link{sprintf}}.
}
\examples{
  nn <- c(0, 1, 0.1, pi,2*pi,-30*pi)
  dc(nn,3)    # "0&0"     "1&0"     "0&100"   "3&142"   "6&283"   "-94&248"
  dcn(nn,3)   # "0&000"   "1&000"   "0&100"   "3&142"   "6&283"   "-94&248"
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{arith}
