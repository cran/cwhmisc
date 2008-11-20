\name{mult.fig.p}
\alias{mult.fig.p}
\title{Plot Setup for MULTiple FIGures, incl. Main Title}
\description{
  Easy Setup for plotting multiple figures (in a rectangular layout) on
  one page.  It allows to specify a main title, a bottom line, and uses \emph{smart}
  defaults for several \code{\link{par}} calls.
}
\usage{
mult.fig.p(nr.plots, mfrow, mfcol,
         marP = rep(0, 4),  mgp = c(1.5, 0.6, 0),
         mar = marP + 0.1 + c(4, 4, 2, 1),
         main = NULL, sub = NULL, adj.sub = 0.5,
         tit.wid = if (is.null(main)) 0 else 1 + 1.5*cex.main,
         quiet = .Device == "postscript",
         cex.main = par("cex.main"),
         col.main = par("col.main"),
         font.main = par("font.main"), \dots)
}
\arguments{
 \item{nr.plots}{integer; the number of plot figures you'll want to draw.}
 \item{mfrow}{\emph{instead} of \code{nr.plots}: integer(2) vector
   giving the rectangular figure layout for \code{\link{par}(mfrow= .)}}
 \item{mfcol}{\emph{instead} of \code{nr.plots}: integer(2) vector
   giving the rectangular figure layout for \code{\link{par}(mfcol= .)}}
 \item{marP}{numeric(4) vector of figure margins to \emph{add}
   (``\bold{P}lus'') to default \code{mar}, see below.}
 \item{mgp}{argument for \code{\link{par}(mpg= .)} with a smaller
 default than usual.}
 \item{mar}{argument for \code{\link{par}(mar= .)} with a smaller
 default than usual, using the \code{marP} argument, see above.}
 \item{main}{character.  The main title to be used for the whole graphic.}
 \item{sub}{character.  The bottom line to be used for the whole graphic.}
 \item{adj.sub}{The value of adj determines the way in which \code{sub}
	 is justified. A value of 0 produces left-justified text, 0.5 centered text and 1 right-justified text. See \code{\link{par}(adj= .)}}
 \item{tit.wid}{numeric; the vertical width to be used for the main
	 title.}
 \item{quiet}{Suppress request to restore graphical parameters.}
 \item{cex.main}{numeric; the character size to be used for the main title.}
 \item{col.main}{string; name of the color to be used for the main title.}
 \item{font.main}{numeric; number of the font to be used for the main title.}
 \item{\dots}{Further arguments to \code{\link{mtext}} for  \code{main}
	 and  \code{sub}.}
}
\value{
  A \code{\link{list}} with two components that are lists themselves, a
  subset of \code{\link{par}()},
  \item{new.par}{the current \code{par} settings.}
  \item{old.par}{the \code{par} \emph{before} the call.}
}
\author{Martin Maechler, \email{maechler@stat.math.ethz.ch},\cr modified by   Christian W. Hoffmann, \email{c-w.hoffmann@sunrise.ch}, \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\seealso{\code{\link{par}}, \code{\link{layout}}.}
\examples{
mult.fig.p(5, main= "Sinus Funktionen verschiedener Frequenzen")
x <- seq(0, 1, len = 201)
for (n in 1:5)
  plot(x, sin(n * pi * x), ylab ="", main = paste("n = ",n))
par(old.par)

rr <- mult.fig.p(mfrow=c(5,1), main= "Sinus Funktionen", cex = 1.5,
               marP = - c(0, 1, 2, 0))
for (n in 1:5)
  plot(x, sin(n * pi * x), type = 'l', col="red", ylab ="")
str(rr)
par(old.par)
## Look at the par setting *AFTER* the above:
str(do.call("par", as.list(names(rr$new.par))))
}
\keyword{hplot}
