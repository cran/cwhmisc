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
capply(str,ff)
cap(char)
capitalize(str)
lower(char)
lowerize(str)
CapLeading(str)
}
\arguments{
  \item{ff}{a function.}
  \item{char}{a single letter.}
  \item{str}{a character vector.}
}
\value{
The same as the argument.
}
\examples{
capitalize  # show use of capply
cap("f")
capitalize(c("TruE","faLSe"))
lower("R")
lowerize("TruE")
CapLeading(c("all you ","need"))  # [1] "All you " "need"
capply(c("abc", "Statistics"), rev)  # reverse strings
}
\note{
  capply has been reverse engineered from the help page on strsplit:
  strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")  can
	be written as  capply(x, rev)
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{print}
