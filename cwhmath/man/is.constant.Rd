\name{is.constant}
\alias{is.constant}
\title{is.constant}
\description{A numerical vector consists only of identical values
}
\usage{
is.constant(x)
}
\arguments{
    \item{x}{a vector}
}
\value{ TRUE if x is numerical and $\max(x) = \min(x)$.
}
\seealso{
  \code{\link{identical}},   \code{\link{all.equal}}
}
\examples{
 is.constant(rep(c(sin(pi/2),1),10)) # TRUE
 x <- factor(c(1,1,NA))
 is.constant(x)            # FALSE because of NA
 is.constant(x[1:2])       # TRUE
 is.constant(c(1,1,NA))    # FALSE because of NA
 is.constant(c(1,1,2))     # FALSE
 is.constant(c(1,1,1))     # TRUE
}
\author{
  Kjetil Brinchmann Halvorsen, \email{kjetil@acelerate.com}, expanded by
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{logic}
