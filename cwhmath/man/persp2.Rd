\name{persp2}
\alias{persp2}
\title{Central perspective from 3 to 2 dimensions along specified axis}
\description{
  Transform by central perspective 3-dimensional data to plane coordinates.
}
\usage{
  persp2(d,axis,r)
}
\arguments{
  \item{d}{Data matrix, one row is one point (x,y,z).}
  \item{axis}{One of 1, 2, 3 to name axis of perspective.}
  \item{r}{The eye point lies at (\code{axis = r, 0, 0}).}}
}
\value{
  The matrix \code{d} with column \code{axis} unchanged, the other two columns
	replaced by their perspective transformation. 
}
\note{
  To avoid ambiguity the column \code{d[,axis]} is retained.
}
\examples{
  par(mfrow=c(1,1))
  x <- c(0,5,NA,0,0,NA,0,0)
  y <- c(0,0,NA,0,6,NA,0,0)
  z <- c(0,0,NA,0,0,NA,0,4)
  d <- cbind(x,y,z)
  dr <- d \%*\% rotm(3,1,3,pi/5) \%*\% rotm(3,1,2,-pi/4) 
  D <- persp2(dr,1,2)
  dx <- D[1:2,]; dy <- D[4:5,]; dz <- D[7:8,]
  plot(0,xlim=range(D[!is.na(D[,2]),2])*1.3,ylim=range(D[!is.na(D[,3]),3])
         *1.3,type="n",xlab="",ylab="")
  lines(dx[,2],dx[,3],col="red")
  lines(dy[,2],dy[,3],col="green")
  lines(dz[,2],dz[,3],col="blue")
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{algebra}
\keyword{dplot}
