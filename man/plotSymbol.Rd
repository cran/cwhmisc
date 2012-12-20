\name{plotSymbols}
\alias{plotSymbols}
\alias{plotSymbolsFonts}
\alias{availColors}
\title{ Plot symbols, colours, and allow to choose }
\description{
  A plot of symbols is generated. By clicking the mouse on a symbol the
  numeric codes are given in ASCII, octal, hex.
  Plot symbols depending on font.
}
\usage{
plotSymbols(interactive=FALSE)
availColors(indx = 0:6)
plotSymbolsFonts(fn=1)
}
\arguments{
  \item{interactive}{allow choice of symbols}
  \item{indx}{indices of panels showing 100 colours each}
  \item{fn}{a font number 1 \dots 5}
}
\note{To turn off the click-bell do 'options(locatorBell=FALSE)' (see ?locator).}
\value{
 list of 
  \item{ch}{character value of symbol}
  \item{dec}{decimal value of symbol}
  \item{hex}{hex value of symbol}
  \item{oct}{octal value of symbol}
}
\examples{
  # A first impression:
  n<-1:34; plot(n,pch=n) # There is a gap between 25 and 34
  plotSymbols(TRUE)
}
\author{Henrik Bengtsson \email{hb@maths.lth.se}, adapted by
  Christian W. Hoffmann, \email{c-w.hoffmann@sunrise.ch}
}
\keyword{color}
\keyword{hplot}
\keyword{iplot}
