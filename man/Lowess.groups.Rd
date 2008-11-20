\name{lowess.bygroup}
\alias{lowess.bygroup}
\alias{loess.bygroup}
\title{Plot data in groups, each with lowess or loess smoothing}
\description{
  data in groups (shown by variable \code{group}) are plotted
}
\usage{
lowess.bygroup(x, y, group, lin=FALSE, col = par("col"), bg = NA,
 pch = par("pch"), cex = 1, ...)
loess.bygroup(x, y, group, lin=FALSE, col = par("col"), bg = NA,
 pch = par("pch"), cex = 1, ...)
}
\arguments{
  \item{x, y}{ coordinate vectors of equal length }
  \item{group}{grouping variable, must be a vector of same length as x and y}
  \item{lin}{If TRUE, then lines connecting the points (x,y) are drawn}
  \item{cex}{see \code{\link{par}}}
  \item{col}{colour of lines}
  \item{bg}{background colour}
  \item{pch}{character to plot}
  \item{\dots}{see \code{pairs}}
}
\value{
 The procedure is called for its side effect of producing a plot
}
\author{
  Christian W. Hoffmann, \email{c-w.hoffmann@sunrise.ch}, \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{hplot}
