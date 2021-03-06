\name{lowess.bygroup}
\alias{lowess.bygroup}
\title{Plot data in groups, each group with separate lowess smoothing}
\description{
  data in groups (shown by variable \code{group}) are plotted
}
\usage{
lowess.bygroup(x, y, group, span=2/3, col=seq_along(x), lty=seq_along(x))
}
\arguments{
  \item{x, y}{ coordinate vectors of equal length }
  \item{group}{grouping variable, must be a vector of same length as x and y}
  \item{span}{span of smooting}
  \item{col}{colour of lines}
  \item{lty}{line type}
}
\value{
 The procedure is called for its side effect of producing a plot
}
\examples{
  par(mfrow=c(1,1))
  gr <- c(rep(1,20),rep(2,30),rep(3,50))
  x <- seq_along(gr); y <- jitter(0.01*(x-50)^2 + 1,1000)
  plot(x,y,pch=".",cex=4,xlab="Lowess, with spans = 0.2 (r,g,mag), 0.4 (blue) ")
  lowess.bygroup(x,y,gr,span=0.2,col=c("red","green","magenta"),lty=rep(2,3))
  lowess.bygroup(x,y,gr,span=0.4,col="blue")
}
\author{
  Christian W. Hoffmann, \email{christian@echoffmann.ch}
}
\keyword{hplot}
