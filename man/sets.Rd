\name{sets}
\alias{setincl}
\title{set inclusion}
\description{
  Check whether one set is included within another.
}
\usage{
  setincl( x, X )
}
\arguments{
  \item{x, X}{sets.}
}
\value{
  \code{TRUE}, if set \code{x} is contained in set \code{X}.
}
\examples{
  setincl(2:3, 1:7) # TRUE
# compare this to:
  grep( "15926",as.character(pi)) == 1 # TRUE
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{arith}
