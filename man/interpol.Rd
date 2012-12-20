\name{interpol}
\alias{setupInterp}
\alias{evalInterp}
\alias{minInterp}
\alias{quadmin}
\title{Polynomial and rational interpolation
}
\description{
Determine the argument of the minimum by polynomial or rational interpolation of given points \code{x, y}.
}
\usage{
setupInterp(x, y, doPoly = TRUE)
evalInterp(xi, ss)
minInterp(x, y, add = FALSE, doPoly = TRUE)
quadmin(x, y)
}
\arguments{
  \item{x}{vector of x-coordinates}
  \item{y}{vector of y-coordinates}
  \item{xi}{argument x of interpolation}
  \item{ss}{setup given by setupInterp}
  \item{add}{if TRUE, one more point is used than for FALSE (default)}
  \item{doPoly}{if TRUE, polynomial interpolation is used, if FALSE, rational interpolation is used, with three points and four points respectively (latter for add=FALSE)}
}
\value{
  \item{setupInterp}{Generate structure for evaluation in \code{evalInterp}}
  \item{xmin}{x-value of the minimum. NA if too few points are given or no minimumm exists in \code{x}.}
}
\references{
Stoer, J., 1989. Numerische Mathematik 1ed. 5. Springer, Berlin.  Applied and Computational Complex Analysis, Vol.2. Wiley, 
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\examples{
x <- c(1,2,4,6); y <- 1/x
xP <- setupInterp(x,y,TRUE)
xT <- setupInterp(x,y,FALSE)
x0 <- seq(0,7,0.1); yP <- evalInterp(x0,xP)
yT <- evalInterp(x0,xT)
plot(x,y,xlim=c(-0.5,7.5),ylim=c(min(y)-2,max(y)+2),cex=2)
lines(x0,yP,col=2,cex=0.5)
lines(x0,yT,col=4,cex=0.5,pch="+")
legend(x="bottom",c("polynomial", "rational"), col = c(2,4),
 text.col= "black", lty = 1, merge = TRUE, bg='white')
minInterp(x,(x-3)^2,add=FALSE,doPoly=TRUE) # 3
minInterp(x,(x+1.0/x),add=FALSE,doPoly=FALSE) # 1 -1
minInterp(x,(x+1.0/x),add=TRUE,doPoly=TRUE) # 8.3471982 0.3194685
}
\keyword{math}
\keyword{symbolmath}
