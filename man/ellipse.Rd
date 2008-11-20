\name{ellipse}
\alias{ellipse}
\alias{conf.ellipse}
\title{Generate ellipses}
\description{
  Given a positive definite symmetric matrix \code{A} of dimension 2 by
	2 and a constant \code{cn}, or the axes \code{a, b} and an angle
	\code{phi} (in radian, counter clockwise), and the midpoint
	coordinates \code{m}, points on the ellipse \eqn{(y-m)'*A^{(-1)}*(y-m) = cn^2} or
	rotm(2,1,2,phi) \%*\% matrix(c(a,0,0,b), 2, 2) will be generated.
}
\synopsis{
  ellipse(k, m, A = NULL, cn = NULL, a = NULL, b = NULL, phi = NULL)
}
\usage{
  ellipse(k, m, A, cn)
  ellipse(k, m, a=, b=, phi=)
  conf.ellipse(k, m, A, df, level = 0.95)
}
\arguments{
  \item{k}{the number of generated points on the ellipse.}
  \item{m}{vector of length 2 containing the midpoint coordinates of the ellipse.}
  \item{A}{positive definite symmetric matrix of dimension 2 by	2}
  \item{cn}{positive constant.}
  \item{a}{major axis}
  \item{b}{minor axis}
  \item{phi}{angle in radian describing the counter clockwise rotation from the x-axis to the major axis.}
	\item{df}{degree of freedom of F-distribution.}
	\item{level}{probability level of F-distribution F(2,df).}
}
\value{
  The matrix with columns consisting of the x and y coordinates of the ellipse.
}
\seealso{
  \code{\link{rotm}} 	
}
\author{Of ellipse: originally Bernhard Flury and Marco Bee for Flury's
  book "A First Course in Multivariate Statistics" (Springer 1997).
  Of conf.ellipse: Roger Koenker \cr
  \email{roger@ysidro.econ.uiuc.edu}, \url{http://www.econ.uiuc.edu} May 19, 1999. \cr
 Refined by: Christian W. Hoffmann <christian.hoffmann@wsl.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\examples{
A <- matrix(c(1,1,1,2), ncol = 2)		 # define matrix A
m <- c(3, 4)					 # define vector m
plot(ellipse(1000,m,A,1))  

plot(pe <- ellipse(800,m,A,1),pch=".",type="n")
lines(rbind(pe,pe[1,]))
lines({pe <- ellipse(600,m,A,0.5); rbind(pe,pe[1,])})
lines({pe <- ellipse(400,m,A,0.25); rbind(pe,pe[1,])})
lines(conf.ellipse(51,m,A,20,0.9),lty=4,col="red")
lines(conf.ellipse(51,m,A,20,0.8),lty=4,col="green")
}
\keyword{multivariate}
\keyword{dplot}
