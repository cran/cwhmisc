\name{plt}
\alias{plt}
\alias{plotTitStamp}
\alias{pltTSV}
\alias{pltCharMat}
\title{Plot depending on switch, Create multiple plots with title and time stamp}
\description{
- \code{plt} executes a (series of) plotting function(s) depending on a
switch, may be useful in \code{\link{source}}.
- \code{plotTitStamp} creates an array of plot panels and executes a
(series of) plotting function(s) and adds a title and a time stamp to the overall plot.
- \code{pltTSV} is a shortcut for \code{\link{plt}(VIEW,plotTitStamp(\dots),file,horizontal)}
- \code{pltCharMat} uses output from \code{\link[CWHstring]{charMat}} to plot numerical matrices as characters}
}
\usage{
plt(VIEW,f,file="",horizontal = FALSE)
plotTitStamp(rows, cols, tit, stampl, f=function(x) 0,
      cex=1.5,reset=TRUE,outer=TRUE,oma=c(2,2,4,2),mar=c(4,4,2,1))
pltTSV(VIEW="see", rows, cols, tit, stampl, f=function(x) 0,
      cex=1.5,reset=TRUE,outer=TRUE,oma=c(2,2,4,2),mar=c(4,4,2,1),file=stampl,horizontal=TRUE)
pltCharMat(m,tit)
}
\arguments{
  \item{VIEW}{"NA" or NA, nothing will be done.
		"see", the plot will be created in the active plot window.
		"eps", the plot will be saved to file "<file>.eps"
	  "ps",  the plot will be created in a postscript file "<file>.ps".
	  "ps+p", like with "ps", the file will also be printed.
	  "pdf", the plot will be created in a pdf file "<file>.pdf"}
  \item{f}{A function to plot the individual plot panels. It can also be
		a statement sequence \{\dots\}}
  \item{file}{Name of file to write to.}
  \item{horizontal}{Landscape, if TRUE.}
  \item{rows}{Number of rows of panels.}
  \item{cols}{Number of columns of panels.}
  \item{tit}{Overall title for plot. A vector of one or two elements. If
  an element is an \code{\link[base]{expression}}, \code{\link[grDevices]{plotmath}} will be used}
  \item{stampl}{Label for time stamp.}
  \item{cex}{Font size used for \code{tit}.}
  \item{reset}{Should previous \code{rows, cols} be restored after execution.}
  \item{outer}{Passed on to mtext.}
  \item{oma}{Outer margin used in initial par(\dots).}
  \item{mar}{Lines of margin used in initial par(\dots).}
  \item{m}{numerical matrix}
}
\value{
	Are called for their side effect to produce a plot and/or generate a file.
}
\note{
	oldpar <- par(mfrow = c(rows, cols),oma=oma,mar=mar) is called at the
beginning of pltTitStamp. Uses
\code{\link[CWHstring:datetime]{datetime}, [lattice:extend.limits]{extend.limits}}.

If you have \code{n} panels you want to plot in a nearly quadratic arrangement, use \code{rows = round(sqrt(n)), cols=ceiling(n/rows)} (tending to slightly "landscape").  This is very similar to \code{\link{n2mfrow}}.
}
\examples{ # see also \code{\link{plotTitStamp}}
x <- rnorm(100); y <- rnorm(100)+1; z <- y+rlnorm(100)
plt("see", {plot(x,y,xlab="this is my x");abline(reg=lm(y~x),lty=2);plot(x,z,pch=3)})
\dontrun{plt("ps", plot(y,x-3*y) )}
\dontrun{plt("ps+p", plot(y,x-3*y) )}
\dontrun{plotTitStamp(1, 2, "1 by 2 plot", "Example 1", {plot(x,y,xlab="my x");
  abline(reg=lm(y~x),lty=2);plot(x,z,pch=3)})}
\dontrun{plotTitStamp(1, 1, "1 by 2 plot", "Ex2", plot(y,x-3*y) )}
  m <- matrix(rnorm(500),nrow=50,ncol=10)
  pltCharMat(m,"Random example")
}
\section{WARNING}{
  The sequence of functions contained in f MUST NOT contain any call to
  \code{\link{postscript}}, because this would try to open another ps device without closing the old one!
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{hplot}
