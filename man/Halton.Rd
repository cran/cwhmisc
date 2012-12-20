\name{Halton}
\alias{Halton}
\alias{HS247}
\title{Halton's quasi-random numbers}
\description{Generating quasi-random numbers by Halton's radical inversion algorithm.
}
\usage{
  HS247(K,N,R,P=rep(0,K))
}
\arguments{
  \item{K}{Integer, number of random sequences}
  \item{N}{Integer, length of the random sequences}
  \item{R}{Integer 1..K, roots of inversion, should be prime}
  \item{P}{Integer 1..K, starting points of inversion}
}
\value{
  A matrix of K columns containing the sequences.
}
\examples{
 par(mfrow=c(2,2))
 n <- 400
 q1 <- HS247(2,400,c(2,2),c(0,pi/10))
 q2 <- HS247(2,400,c(2,3))
 q3 <- HS247(2,400,c(2,5))
 q4 <- HS247(2,400,c(17,19)) # prone to correlations
 plot  (q1,pch="+",col="blue",cex=0.5)
 points(q2,pch=4,col="green",cex=0.5)
 plot  (q2,pch=4,col="green",cex=0.5)
 points(q3,pch=":",col="red")
 points(q4,pch=4,col="magenta",cex=0.5)
 plot  (q2,pch=4,col="green",cex=0.5)
 points(HS247(2,400,c(2,3),c(pi/10,pi/10)),pch=5,cex=0.5,col="red")
 plot  (HS247(2,400,c(2,3),c(pi/10,pi/10)),pch=5,cex=0.5,col="red")
 points(HS247(2,400,c(2,3),c(pi/10+0.2,pi/10+0.2)),pch="-")
}
\source{
  J. H. Halton, 1964. Algorithm 247: Radical-inverse quasi-random point sequence, Communications of the ACM, Vol.7,12, pp. 701 - 702 .
}
\references{
  http://en.wikipedia.org/wiki/Halton_sequences
}
\author{Christian W. Hoffmann, \email{hoffmann@wsl.ch}}
\keyword{distribution}


