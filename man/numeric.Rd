\name{numericString}
\alias{numericString}
\alias{allDigits}
\alias{intToASCII}
\alias{intToBase}
\alias{intToOct}
\alias{intToHex}
\title{Test string for being a number or made up of digits only, convert to bases.}
\description{
 Functions for testing strings and for conversion of integers to bases other than decimal as string representations.
}
\usage{
numericString(str)
allDigits(str)
intToASCII(i)
intToBase(i,Base=2)
intToOct(i)
intToHex(i)
}
\arguments{
  \item{str}{A character vector.}
  \item{i}{Integer number to be converted.}
  \item{Base}{Number base to be converted to.}
}
\details{
  \code{numericString} Test whether the elements of a character vector
	represent legal numbers only.
  \code{allDigits} Test whether the elements of a character vector consist of digits only.
  \code{intToASCII} Show character or octal representation at a place in the ASCII sequence.
  \code{intToBase} Convert an integer number to string representation in a base between 2 and 16 inclusive.
  \code{intToOct} Convert an integer number to string representation in octal.
  \code{intToHex} Convert an integer number to string representation in hexadecimal.
}
\value{
	TRUE, FALSE, string representations
}
\examples{
allDigits(c("1231","89a8742")) #  TRUE FALSE
numericString(c("1231","8.9e-2",".7d2")) # [1]  TRUE  TRUE FALSE
intToASCII(1:255)
sapply(1:50,intToBase,2)
sapply(1:50,intToBase,7)
sapply(1:50,intToOct)
sapply(1:50,intToHex)
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{character}
\keyword{arith}

