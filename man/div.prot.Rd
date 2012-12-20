\name{div.prot}
\alias{div.prot}
\title{Protected division}
\description{
  num/den, but num/0 -> .Machine$double.xmax^(3/4)
}
\usage{
  div.prot(num,den)
}
\arguments{
  \item{den,num}{real, numerator and denominator}
}
\value{
  num/den, if is.infinite(num/den) then .Machine$double.xmax^(3/4), the ^(3/4) for getting something well below Inf.
}
\examples{
  de <- .Machine$double.eps
  v<-c(0,de/c(1,2,4,8))
  div.prot(1,v)
 #  1.55252e+231  4.50360e+15  9.00720e+15  1.80144e+16  3.60288e+16
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{algebra}
