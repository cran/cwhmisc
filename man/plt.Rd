\name{plt}
\alias{pltCharMat}
\alias{pltRCT}
\alias{histRCT}
\alias{SplomT}
\title{Plot depending on switch, Create multiple plots with title and time stamp}
\description{
- \code{pltCharMat} uses output from \code{\link{charMat}} to plot numerical matrices as characters.
- \code{pltRCT} executes a (series of) plotting function(s) under the control of some useful switches, may be useful in \code{\link{source}}.
- \code{histRCT} creates a (series of) histogram(s), uses \code{pltRCT}.
- \code{SplomT} creates a scatterplot matrix with a) covariances (with
script size proportional to size) in the upper triangle, b) histograms
(with smoothing) and variable names in the diagonal, and c) scatterplot
with smoothes in y and x direction in the lower triangle, stressing high
correlations by nearly parallel lines. See figure in other documentation.
}
\usage{
pltCharMat(m,...)
pltRCT(rows, cols, tit="", f = function(x) 0, cex = 1.5, 
    reset = TRUE, outer = TRUE, oma = c(2, 2, 4, 2), mar = c(4, 4, 2, 1))
histRCT(data, rows = round(sqrt(ncol(data))), 
    cols = ceiling(ncol(data)/rows), breaks = "Sturges",
      mainL = deparse(substitute(data)), mainC = colnames(eval(substitute(data))))
SplomT(data, mainL = deparse(substitute(data)), xlabL = "", 
    hist = "h", adjust = 1, hist.col = trellis.par.get("strip.background")$col[5],
      cex.diag = 1, h.diag=0.4, colYonX = "red", colXonY = "blue", ...)
}
\arguments{
  \item{m}{Numerical matrix}
  \item{tit}{Overall title for plot. A vector of one or two elements. If
  an element is an \code{\link[base]{expression}}, \code{\link[grDevices]{plotmath}} will be used.}
  \item{rows}{Number of rows of panels}
  \item{cols}{Number of columns of panels}
  \item{f}{A function to plot the individual plot panels. It can also be
		a statement sequence \{\dots\}.}
  \item{cex}{Font size used for \code{tit}}
  \item{reset}{Should previous \code{rows, cols} be restored after execution. See \code{note}}
  \item{outer}{Passed on to mtext.}
  \item{oma}{Outer margin used in initial par(\dots).}
  \item{mar}{Lines of margin used in initial par(\dots).}
  \item{data}{Matrix or dataframe containing data, varibles in columns}
  \item{breaks}{Breaks for histogram}
  \item{mainL}{Label on top of scatterplot matrix or matrix of histograms}
  \item{mainC}{Labels on top of each of the histograms, should be character
  vector of length = number of columns of data}
  \item{xlabL}{Label for x axis}
  \item{hist}{"h" = histogram, "d" = density curve, "b" = both}
  \item{adjust}{factor to adjust smoothing window for density curve}
  \item{hist.col}{colour for the bars of the histograms}
  \item{cex.diag}{correction factor for font height of correlations and
	names in the diagonal}
  \item{h.diag}{placement of the variable name in the diagonal panel, =0
	means on the lower border, = 0.5 in the middle between lower and
	upper border}
  \item{colYonX, colXonY}{colour of smoothing lines, y on x and x on y}
  \item{\ldots}{Parameters passed on to upper.panel,lower.panel,diag.panel}
}
\value{
	These functions are called for their side effect to produce a plot.
}
\note{
	oldpar <- par(mfrow = c(rows, cols),oma=oma,mar=mar) is called at the
beginning of pltRCT. Uses \code{\link[lattice]{splom}}, [lattice:extend.limits]{extend.limits}, and \code{\link{datetime} }.

If you have \code{n} panels you want to plot in a nearly quadratic arrangement, use \cr \code{rows = round(sqrt(n)), cols=ceiling(n/rows)} (tending to slightly "landscape").  This is very similar to \code{\link{n2mfrow}}.
  histRCT drops columns with less than 2 legal (non-NA)
  values. For empty matrices no plot will be generated.
}
\examples{
  x <- rnorm(100); y <- rnorm(100)+1; z <- y+rlnorm(100)
  pltRCT(1,1,f={plot(x,y,xlab="data with trend");
    abline(reg=lm(y~x),lty=2);points(x,z,pch=3)})
  nr <- 100; nc <- 8;
  data <- as.data.frame(matrix(rnorm(nr*nc),nrow=nr,ncol=nc))
  data[,nc]   <- data[,nc-2] + 0.3*data[,nc-1] #generate higher correlations
  data[,nc-1] <- data[,nc-1] + 0.9*data[,nc]
  colnames(data)<-paste("vw",letters[1:nc],sep="")
  SplomT(data,mainL="",hist="d",cex.diag=0.6,hist.col="green")
  SplomT(data,mainL="",hist="b",adjust=0.4,cex.diag = 0.5)
  pltRCT(1, 1, tit="1 by 1 plot", f=plot(y,x-3*y) )
  nr <- 25;  nc <- 16
  pltRCT(1, 2, f={plot(x,y,xlab="my x")
    m <- matrix(rnorm(nr*nc),nrow=25,ncol=nc)
    pltCharMat(m,cex=0.5,col="red")
  })

}
\section{WARNING}{
  The sequence of functions contained in \code{f} MUST NOT contain any call to
  \code{\link{postscript}}, because this would try to open another ps device without closing the old one!
}
\author{
  Christian W. Hoffmann, \email{christian@echoffmann.ch}, with the assistance of
  Deepayan Sarkar \email{Deepayan.Sarkar@r-project.org}.
}
\keyword{hplot}


