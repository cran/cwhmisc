\name{T3plot}
\alias{T3plot}
\title{T3plot}
\description{
  T3 plot for a graphical check on normality together with 95\%- and
  99\%-acceptance regions. If the black line does not cross either the
  5\% nor the 1\% line, the input data are normal with less than 1\% error.
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
  T3plot(rnorm(100))
  \dontrun{T3plot(rnorm(10000)) }
  \dontrun{T3plot(rnorm(1000)+runif(1000)*0.1) }
  \dontrun{T3plot(rnorm(1000)+runif(1000)*10)}
}
\references{Ghosh, S. (1996) A new graphical tool to detect non-normality. Journal of the Royal Statistical  Society B , 58, 691-702. }
\author{Sucharita Ghosh, \email{rita.ghosh@wsl.ch}, \cr
  \url{http://www.wsl.ch/personal_homepages/ghosh/index_EN?redir=1&}, \cr
 with cosmetics by 
  Christian W. Hoffmann, \email{c-w.hoffmann@sunrise.ch}, \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN?-C=&n}
}
\keyword{distribution}