\name{ellipse}
\alias{ellipse}
\alias{conf.ellipse}
\title{Generate ellipses}
\description{
  Given the axes \code{a, b} and an angle \code{phi} (in radian, counter clockwise), and the midpoint
	coordinates \code{m}, points on the ellipse \eqn{(y-m)'*A^{(-1)}*(y-m) = cn^2} with \eqn{A =	rotL(phi,2,1,2) * matrix(c(a,0,0,b), 2, 2)} will be generated, see \code{\link{rotL}}.
}
\usage{
  ellipse(k, m, a, b, phi, mlt = 1.0 )
  conf.ellipse(k, m, a, b, phi, df1, df2, level = 0.95)
}
\arguments{
  \item{k}{the number of generated points on the ellipse.}
  \item{m}{vector c(x, y) containing the midpoint coordinates of the ellipse.}
  \item{mlt}{(positive) expansion factor for axes.}
  \item{a}{major axis}
  \item{b}{minor axis}
  \item{phi}{angle in radian describing the counter clockwise rotation from the x-axis to the major axis.}
  \item{df1, df2, level}{degrees of freedom and probability level of F-distribution.}
}
\value{
  \code{ellipse} The matrix with columns consisting of the x and y coordinates of the ellipse.
  \code{conf.ellipse} The matrix with columns consisting of the x and y coordinates of the confidence ellipse according to \code{qf(level, df1, df2)}, see \code{\link{qf}}.
}
\author{ Christian W. Hoffmann <christian.hoffmann@wsl.ch> }
\examples{
  m <- c(2,7); a<-5; b<-2; phi<-pi/7
  df1 <- 2; df2 <- 20
# show F for different confidence levels:
  p <- c(0.5, 0.75, 0.8, 0.95)
  qf(p, df1, df2) #  0.717735 1.486984 1.746189 3.492828
 # error in check but not in direct execution
  el7 <- conf.ellipse(60,m,a,b,phi,df1,df2,p[2])
  plot(el7,type="n",xlab="",ylab="")
  lines(conf.ellipse(60,m,a,b,phi,df1,df2,p[1]),lty=2,col="red")
  lines(conf.ellipse(60,m,a,b,phi,df1,df2,p[3]),lty=2,col="green")
  lines(conf.ellipse(60,m,a,b,phi,df1,df2,p[4]),lty=2,col="blue")
  lines(el7,lty=2,col="orange")
  legend(x="bottom",paste(as.character(p*100),rep("\%",length(p)),
    sep=""), col = c("red", "orange","green","blue"), text.col=
    "black", lty = c(2,2,2,2), merge = TRUE, bg='white', cex=0.9)
}
\keyword{multivariate}
\keyword{dplot}
