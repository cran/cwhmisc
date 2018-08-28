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
 q1 <- HS247(2,n,c(2,2),c(0,pi/10))
 q2 <- HS247(2,n,c(2,3))
 q3 <- HS247(2,n,c(2,5))
 q4 <- HS247(2,n,c(17,19)) # prone to correlations
 q5 <- HS247(2,n,c(2,3),c(pi/10,pi/10))
 of <- 0.2
 q6 <- HS247(2,n,c(2,3),c(pi/10+of,pi/10+of))
 \dontrun{
   plot  (q1,pch="+",col="blue",cex=0.5,xlab="roots = (2,2), +blue, green")
   points(q2,pch=4, col="green",cex=0.5)
   plot  (q2,pch=4,col="green",cex=0.5,xlab="roots = (2,3),
          :green, (2,5) :red, (17,19) magenta")
   points(q3,pch=":",col="red")
   points(q4,pch=4,col="magenta",cex=0.5)
   plot  (q2,pch=4,col="green",cex=0.5,xlab="roots = 2, 2, green, red")
   points(q5,pch=5,cex=0.5,col="red")
   plot  (q5,pch=5,cex=0.5,col="red",xlab="roots = 2, 3, red")
   points(q6,pch="-")
 }
}
\note{
Halton, J.H., Smith, G.G., 1961. Algorithm 247
Radical-inverse quasi-random point sequence
 Computes a sequence of N quasi-random points
 lying in the K-dimensional unit cube given by
 0 < x_i < 1, i = 1,2,..,K. The i-th component
 of the m-th point is stored in Q[m,i].
 The sequence is initiated by a zero-th point
 stored in P, and each component sequence is
 iteratively generated with parameter R[i].
 E is a positive error-parameter.
 K, N, E, P[i], R[i], i=1..K, are to be given.
}
\source{
  J. H. Halton, 1964. Algorithm 247: Radical-inverse quasi-random point sequence, Communications of the ACM, Vol.7,12, pp. 701 - 702 .
}
\references{
  http://en.wikipedia.org/wiki/Halton_sequences
}
\author{Christian W. Hoffmann, \email{hoffmann@wsl.ch}}
\keyword{distribution}


