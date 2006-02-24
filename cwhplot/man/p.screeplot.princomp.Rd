\name{p.screeplot.princomp}
\alias{p.screeplot.princomp}
\title{Plot screeplot}
\description{
  \code{p.screeplot.princomp} plots a screeplot with labels of 	cumulative variances
}
\synopsis{
p.screeplot.princomp(object, variables=seq(length(object$sdev)), cumulative=TRUE, main=deparse(substitute(object)), ylim=c(0,max(vars)+0.05),...)
}
\arguments{
  \item{object}{an object of class "princomp" created by \code{\link{princomp}}}
  \item{variables}{a vector of integers selecting the elements of $object\$sdev^2$.}
  \item{cumulative}{a logical value. If FALSE, the labels on bars show variances. If TRUE, they show cumulative variances.}
  \item{main}{overall title for the plot.}
  \item{ylim}{limits for the y axis.}
  \item{\dots}{optional arguments for \code{\link{barplot}}}
}
\value{
A numeric vector (or matrix, when beside = TRUE), say mp, giving the coordinates of all the bar midpoints drawn, useful for adding to the graph.	
}
\examples{
  library(stats)
  m <- cbind(rnorm(100),rlnorm(100))
  p.screeplot.princomp(princomp(m))
\dontrun{p.screeplot.princomp(princomp(m),main="Screeplot",ylab="Variances",cex=0.6)}
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \newline
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{multivariate}
