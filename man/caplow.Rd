\name{cap}
\alias{capply}
\alias{cap}
\alias{capitalize}
\alias{lower}
\alias{lowerize}
\alias{CapLeading}
\alias{strReverse}
\title{Functions for strings}
\description{
  \code{capply} Apply function to elements in character vector (utility function)
  \code{cap} and \code{capitalize} change to capital letters.
  \code{lower} and \code{lowerize} change to lower case letters.
  \code{CapLeading} Capitalizes the first character of each element of a
	character vector
	
}
\usage{
capply(str,ff,...)
cap(char)
capitalize(str)
lower(char)
lowerize(str)
CapLeading(str)
strReverse(str)
}
\arguments{
  \item{str}{a character vector.}
  \item{ff}{a function.}
  \item{...}{additional parameters for function ff.}
  \item{char}{a single letter.}
  \item{strReverse}{the reverse of \code{str}}
}
\value{
The same type as the argument.
}
\examples{
  # capitalize shows the use of capply
  cap("f")    # "F"
  capitalize(c("TruE","faLSe"))  # "TRUE"  "FALSE"
  lower("R")  # "r"
  lowerize("TruE")  # "true"
  CapLeading(c("all you ","need"))     # "All you " "Need"
  capply(c("abc", "elephant"), rev)  # "cba"    "tnahpele"
}
\note{
  capply has been reverse engineered from the help page on strsplit.
  strReverse <- function(x)  capply( x, rev )
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{print}
