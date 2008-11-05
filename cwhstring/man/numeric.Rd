\name{numeric_string}
\alias{numeric_string}
\alias{all_digits}
\alias{intToASCII}
\alias{intToBase}
\alias{intToOct}
\alias{intToHex}
\title{Test string for being a number or made up of digits only, convert to bases.}
\description{
  \code{numeric_string} Test whether the elements of a character vector
	represent legal numbers only.
  \code{all_digits} Test whether the elements of a character vector consist of digits only
  \code{intToASCII} Show character or octal representation at a place in the ASCII sequence
  \code{intToBase } Convert an integer number to string representation in a base between 2 and 16 inclusive
  \code{intToOct } Convert an integer number to string representation in octal
  \code{intToHex } Convert an integer number to string representation in hexadecimal
}
\usage{
numeric_string(str)
all_digits(str)
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
\value{
	TRUE, FALSE
  string representation
}
\examples{
all_digits(c("1231","89a8742")) #  TRUE FALSE
numeric_string(c("1231","8.9e-2",".7d2")) # [1]  TRUE  TRUE FALSE
intToASCII(1:255)
sapply(1:50,intToBase,2)
sapply(1:50,intToBase,7)
sapply(1:50,intToOct)
sapply(1:50,intToHex)
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{character}
\keyword{arith}

