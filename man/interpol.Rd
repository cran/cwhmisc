\name{interpol}
\alias{setupInterp}
\alias{evalInterp}
\alias{minInterp}
\alias{quadmin}
\alias{lerp}
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
lerp(p1, p2, t)
}
\arguments{
  \item{x}{vector of x-coordinates}
  \item{y}{vector of y-coordinates}
  \item{xi}{argument x of interpolation}
  \item{p1,p2}{point coordinates for linear interpolation}
  \item{t}{0 <= t <= 1, linear interpolation distance}
  \item{ss}{setup given by setupInterp}
  \item{add}{if TRUE, one more point is used than for FALSE (default)}
  \item{doPoly}{if TRUE, polynomial interpolation is used, if FALSE, rational interpolation is used, with three points and four points respectively (latter for add=FALSE)}
}
\value{
  \item{setupInterp}{Generate structure \code{ss} for evaluation in \code{evalInterp}}
  \item{minInterp, quadmin}{x-value of the minimum. NA if too few points are given or no minimumm exists in \code{x}.}
  \item{lerp}{linerly interpolated point, t=0 -> p1, t=1 -> p2}
}
\references{
Stoer, J., 1989. Numerische Mathematik 1ed. 5. Springer, Berlin.  Applied and Computational Complex Analysis, Vol.2. Wiley, 
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\examples{
  opar <- par(mfrow=c(2,2))
  x <- c(1,2,4,6); y <- 1/x
  pint <- function( x, y, add, dopoly, ylab="" ) {
    print(paste(" minimum at = ", minInterp(x,y,add=add,doPoly=dopoly) ) )
    xP <- setupInterp(x,y,TRUE)
    xT <- setupInterp(x,y,FALSE)
    x0 <- seq(0,7,0.1); yP <- evalInterp(x0,xP)
    yT <- evalInterp(x0,xT)
    plot(x,y,xlim=c(-0.5,7.5),ylim=c(min(y)-2,max(y)+2),cex=2,ylab=ylab)
    lines(x0,yP,col=2,cex=0.5)
    lines(x0,yT,col=4,cex=0.5,pch="+")
    legend(x="bottom",c("polynomial", "rational"), col = c(2,4),
     text.col= "black", lty = 1, merge = TRUE, bg='white')
  }
  pint(x,y,add=FALSE,dopoly=TRUE,"1/x") # 6 ?? = minimum
  pint(x, (x-3)^2,add=FALSE,dopoly=TRUE,"(x-3)^2") # 3
  pint(x,x+1.0/x,add=FALSE,dopoly=FALSE,"x+1.0/x dopoly=F") # 1 -1
  pint(x,x+1.0/x,add=TRUE,dopoly=TRUE,"x+1.0/x dopoly=T") # 8.3471982 0.3194685
  par(opar)
  
}
\keyword{math}
\keyword{symbolmath}
