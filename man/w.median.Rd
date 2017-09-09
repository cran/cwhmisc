\name{w.median}
\alias{w.median}
\title{Weighted median}
\description{
 Compute the weighted median.
}
\usage{
 w.median (x,w)
}
\arguments{
  \item{x,w}{Real, data and weights}
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\seealso{\code{\link{median}}, \code{\link{quantile}}}
\examples{
w.median(c(7,1,2,4,10,15),c(1,1/3,1/3,1/3,1,1)) #  7
w.median(c(1,2,4,7,10,15),c(1/3,1/3,1/3,1,1,1)) #  7
w.median(c(7,7/3,10,15)) # 7
     # '1','2','4 of weights='1/3' are replaced by '7/3' (weight=1)
w.median(c(7,1,2,4,10),c(1,1/3,1/3,1/3,1)) #  7
w.median(c(7,1,2,4,10)) #  4
w.median(c(7,1,NA,4,10),c(1,1/3,1/3,1/3,1)) # 7
}
\keyword{robust}
