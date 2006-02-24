\name{weighted.mean1}
\alias{weighted.mean1}
\title{Weighted mean1}
\description{
  Compute the (trimmed) arithmetic mean of the vector of values given as its
	first argument. The weights are given in the second argument.
}
\usage{
weighted.mean1(x, w=NULL, trim = 0, na.rm=FALSE)
}
\arguments{
  \item{x}{a numeric vector containing the values whose mean is to be computed.}
  \item{w}{a numeric vector containing the weights.}
  \item{trim}{the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is
       computed.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
}
\value{
If \code{trim} is zero (the default), the arithmetic mean of the values in x is computed. 
If \code{trim} is non-zero, a symmetrically trimmed mean is computed
with a fraction of trim observations deleted from each end before the mean is computed.
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\seealso{\code{\link{weighted.mean}}, \code{\link{mean}}, \code{\link{quantile}}}
\examples{
weighted.mean1(c(7,1,2,4,10,15),c(1,1/3,1/3,1/3,1,1)) # 8.583333333
weighted.mean1(c(1,2,4,7,10,15),c(1/3,1/3,1/3,1,1,1)) # ordered differently  8.583333333
weighted.mean1(c(7,7/3,10,15)) # same as previous, but unweighted:
         # '1','2','4 of weights='1/3' are replaced by '7/3' (weight=1)

weighted.mean1(c(7,1,2,4,10),c(1,1/3,1/3,1/3,1)) #  6.444444444
weighted.mean1(c(7,1,2,4,10)) #  4.8
weighted.mean1(c(7,1,NA,4,10),c(1,1/3,1/3,1/3,1),na.rm =TRUE) # 7
}
\keyword{distribution}
