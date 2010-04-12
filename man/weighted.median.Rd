\name{weighted.median}
\alias{weighted.median}
\title{Weighted median}
\description{
  Compute the sample median of the vector of values given as its
	first argument. The weights are given in the second argument; if
	given, must be of the same length as first argument.  
}
\usage{
weighted.median(x, w=NULL, na.rm=FALSE)
}
\arguments{
  \item{x}{a numeric vector containing the values whose median is to be computed.}
  \item{w}{a numeric vector containing the weights.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
}
\value{
  Unweighted mean, if weights  \code{w} are not given. Otherwise, value of
  \code{x} nearest to the position, where the cumulative sum of the
  weights reaches 50\% of its maximum value.

  Special cases are considered as
  closely as possible, see code.
}
\author{
  Christian W. Hoffmann, \email{c-w.hoffmann@sunrise.ch}
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
