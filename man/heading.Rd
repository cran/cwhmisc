\name{heading}
\alias{heading}
\title{Write a line of text with underlining and blank lines. Useful for line oriented output.}
\usage{
  heading(text, comm = "", ch = "-", m = 1, n = 0)
}
\arguments{
	\item{text}{(one line) text string, if "": only m+n empty lines}
	\item{comm}{optional comment character written at start of text and of underline}
	\item{ch}{character used to underline text, if "": no underline}
	\item{m}{number of empty lines written before text}
	\item{n}{number of empty lines written after underlined text}
}
\description{
  \code{heading} writes m blank lines, then \code{text}, 
  underline with \code{length(text)} copies of \code{ch}
  then write \code{n} blank lines
  \code{comm} can be used as comment character
}
\value{
  NULL 
}
\examples{
#\#  "#|" means the left margin
heading("Very compact","","=",0,1)
#\#| 
heading("","=")  # one blank line only
#\#|
heading("standard")
#\#|
#\#|standard
#\#|--------
heading("Just a line, not really a heading","","",0,0)
#\#|Just a line, not really a heading
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{print}
            
