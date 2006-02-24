\name{pointfit}
\alias{pointfit}
\title{Least squares fit of point clouds, or the Procrustes problem.}
\description{
  Find a transformation which consists of a translation \code{tr} and a
	rotation \code{Q} multiplied by a positive scalar \code{f} which maps
	a set of points \code{x} into the set of points \eqn{xi: xi = f * Q \%*\% 
	x + tr} The resulting error is minimized by least squares.
}
\usage{
  pointfit(xi,x)
}
\arguments{
  \item{x}{Matrix of points to be mapped. Each row corresponds to one point.}
  \item{xi}{Matrix of target points. Each row corresponds to one point.}
}
\details{
  The optimisation is least squares for the problem xi =. Q \%*\% 
	x + tr. The expansion factor \code{f} is computed as the
	geometric mean of the quotients of corresponding
	coordinate pairs. See the program code.
}
\value{
  A list containing the following components:
  \item{Q}{The rotation.}
  \item{f}{The expansion factor.}
  \item{tr}{The translation vector.}
  \item{res}{The residuals xi - f * Q \%*\% x + tr.}
}
\references{"Least squares fit of point clouds" in: W. Gander and J. Hrebicek, ed., Solving Problems in Scientific Computing using Maple and Matlab, Springer Berlin Heidelberg New York, 1993, third edition 1997.}
\author{Walter Gander, \email{gander@inf.ethz.ch}, \cr
 \url{http://www.inf.ethz.ch/personal/gander/}}
\seealso{ \code{\link{rotm}} to generate rotation matrices (mathlib),
 \code{\link{rotangle}} to determine them from orthogonal matrix.}
\examples{
  # nodes of a pyramid
  A <- matrix(c(1,0,0,0,2,0,0,0,3,0,0,0),4,3,byrow=TRUE)
  nr <- nrow(A)
  v <- c(1,2,3,4,1,3,4,2)  # edges to be plotted
#  plot
  # points on the pyramid
  x <-
matrix(c(0,0,0,0.5,0,1.5,0.5,1,0,0,1.5,0.75,0,0.5,2.25,0,0,2,1,0,0),
    7,3,byrow=TRUE)
  # simulate measured points
  # thetar <- runif(3)
  thetar <- c(pi/4, pi/15, -pi/6)
  # orthogonal rotations to construct Qr
  Qr <- rotm(3,1,2,thetar[3]) \%*\% rotm(3,1,3,thetar[2]) \%*\% rotm(3,2,3,thetar[1])
  # translation vector
  # tr <- runif(3)*3
  tr <- c(1,3,2)
  # compute the transformed pyramid
  fr <- 1.3
  B <- fr * A \%*\% Qr + outer(rep(1,nr),tr)
  # distorted points
  # xi <- fr * x + outer(rep(1,nr),tr) + rnorm(length(x))/10
  xi <- matrix(c(0.8314,3.0358,1.9328,0.9821,4.5232,2.8703,1.0211,3.8075,1.0573,
0.1425,4.4826,1.5803,0.2572,5.0120,3.1471,0.5229,4.5364,3.5394,1.7713,
3.3987,1.9054),7,3,byrow=TRUE)
  (pf <- pointfit(xi,x))
  # the fitted pyramid
  (C <- A \%*\% pf$Q + outer(rep(1,nrow(A)),pf$tr))
  (theta <- rotangle(pf$Q))
  thetar
  # as a final check we generate the orthogonal matrix S from the computed angles
  # theta and compare it with the result pf$Q
  Ss <- rotm(3,1,2,theta[3]) \%*\% rotm(3,1,3,theta[2]) \%*\% rotm(3,2,3,theta[1])
  max(svd(Ss*pf$factor-pf$Q)$d) #>  4.84082e-16  but why pf$factor ??
}
\keyword{algebra}

