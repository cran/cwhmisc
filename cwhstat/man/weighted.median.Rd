\name{weighted.median}
\alias{weighted.median}
\title{Weighted median}
\description{
  Compute the sample median of the vector of values given as its
	first argument. The weights are given in the second argument.
}
\usage{
weighted.median(x, w=NULL, na.rm=FALSE)
}
\arguments{
  \item{x}{a numeric vector containing the values whose median is to be computed.}
  \item{w}{a numeric vector containing the weights.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\seealso{\code{\link{median}}, \code{\link{quantile}}}
\examples{
weighted.median(c(7,1,2,4,10,15),c(1,1/3,1/3,1/3,1,1)) # 8.5 [even number]
weighted.median(c(1,2,4,7,10,15),c(1/3,1/3,1/3,1,1,1)) # ordered differently  8.5
weighted.median(c(7,7/3,10,15)) # same as previous, but unweighted:
         # '1','2','4 of weights='1/3' are replaced by '7/3' (weight=1)

weighted.median(c(7,1,2,4,10),c(1,1/3,1/3,1/3,1)) #  7
weighted.median(c(7,1,2,4,10)) #  4
weighted.median(c(7,1,NA,4,10),c(1,1/3,1/3,1/3,1),na.rm =TRUE) # 8.5}
\keyword{distribution}
