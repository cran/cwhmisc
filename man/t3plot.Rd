\name{T3plot}
\alias{T3plot}
\title{T3plot}
\description{
  T3 plot for a graphical check on normality together with 95\%- and
  99\%-acceptance regions. If the black line does not cross either the
  5\% nor the 1\% line, the input data are normal with less than 1\% error.\cr
}
\usage{
  T3plot(x,lab=paste("T3 plot of ",deparse(substitute(x))),
  legend.pos="bottom", cex=0.6, ...)
}
\arguments{
  \item{x}{Data vector.}
  \item{lab}{String for heading of plot.}
  \item{legend.pos, cex, ...}{see \code{\link{legend}}.}
 }
\value{
  Is called for its side effect to produce a T3 plot.
}
\examples{
  par(mfrow=c(2,2))
  T3plot(rnorm(100))
  T3plot(rnorm(10000))
  T3plot(rnorm(1000)+runif(1000)*0.1,"Rather well normal")
  T3plot(rnorm(1000)+runif(1000)*10,"Not < 1 percent error for normality")
}
\references{Ghosh, S. (1996) A new graphical tool to detect non-normality. Journal of the Royal Statistical  Society B , 58, 691-702. }
\author{Sucharita Ghosh, \email{rita.ghosh@wsl.ch}, \cr
  with cosmetics by  Christian W. Hoffmann, \email{christian@echoffmann.ch}
}
\keyword{distribution}
