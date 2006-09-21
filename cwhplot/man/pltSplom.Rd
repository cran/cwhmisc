\name{pltSplomT}
\alias{pltHist}
\alias{pltSplomT}
\title{Plot histograms, scatterplot matrix }
\description{
  \code{pltHist} plots histograms of the columns of a data frame or
  matrix, in a rectangular fashion.
  \code{pltSplomT} plots a scatterplot matrix analogous to \code{pairs},
  with histograms or density estimation in the diagonal, correlations in
  scaled font, and scatterplots in the lower diagonal, with smoothing in
  both directions, with title and time stamp.
}
\usage{
  pltHist(data,rows=round(sqrt(ncol(data))),cols=ceiling(ncol(data)/rows))
  pltSplomT(data, mainL = deparse(substitute(data)), xlabL = "", 
    hist = c("h", "d", "b"), adjust = 1, hist.col =
trellis.par.get("strip.background")$col[5], cex.diag = 1,  h.diag=0.4,
colYonX = "red", colXonY = "blue", ...)
}
\arguments{
  \item{data}{Matrix or data frame containing data, varibles in columns}
  \item{mainL}{Label on top}
  \item{xlabL}{Label for x axis}
  \item{\ldots}{Parameters passed on to upper.panel,lower.panel,diag.panel}
  \item{rows, cols}{Number of rows and columns for output figures like in
    \code{\link[graphics]{mfreg}}}
  \item{hist}{"h" = histogram, "d" = density curve, "b" = both}
  \item{adjust}{factor to adjust smoothing window for density curve}
  \item{hist.col}{colour for the bars of the histograms}
  \item{cex.diag}{correction factor for font height of correlations and
	names in the diagonal}
  \item{h.diag}{placement of the variable name in the diagonal panel, =0
	means on the lower border, =0.5 in the middle between lower and
	upper border}
  \item{colYonX, colXonY}{colour of smoothing lines, y on x and x on y}
}
\value{
	Are called for their side effect to produce a plot.
}
\examples{ # see also
\dontrun{
  data <- as.data.frame(matrix(rnorm(500),nrow=100,ncol=5))
  colnames(data)<-letters[1:5]
  pltSplomT(data,mainL="",hist="d",adjust=1.0,hist.col="green")
  pltSplomT(data,mainL="",hist="b",adjust=0.4,cex.diag = 1.9)
  pltHist(data)
} }
\note{
  Uses \code{\link[lattice]{splom}} in package "lattice",
  \code{\link[CWHplot]{datetime} in "CWHplot".
} }
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann} with the assistance of
  Deepayan Sarkar \email{deepayan@cs.wisc.edu}.
}
\keyword{hplot}
