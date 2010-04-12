\name{cap}
\alias{capply}
\alias{cap}
\alias{capitalize}
\alias{lower}
\alias{lowerize}
\alias{CapLeading}
\title{Change case of strings}
\description{
  \code{capply} apply function to elements in character vector (utility function)
  \code{cap} and \code{capitalize} change to capital letters.
  \code{lower} and \code{lowerize} change to lower case letters.
  \code{CapLeading} Capitalizes the first character of each element of a
	character vector
}
\usage{
capply(str,ff,\ldots)
cap(char)
capitalize(str)
lower(char)
lowerize(str)
CapLeading(str)
}
\arguments{
  \item{str}{a character vector.}
  \item{ff}{a function.}
  \item{char}{a single letter.}
  \item{\ldots}{a function .}
}
\value{
The same as the argument.
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
  capply has been reverse engineered from the help page on strsplit:
  strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")  can
	be written as  capply(x, rev)
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{print}