\name{digits}
\alias{allDigits}
\alias{isNumeric}
\alias{str2dig}
\title{Test, convert numbers}
\description{
 Test, convert numbers
}
\usage{
  allDigits( str, base=10 )
  isNumeric(str)
  str2dig( str )
}
\arguments{
  \item{str}{Vector of strings}
  \item{base}{Integer, base of number representation used in \code{\link{r2B}} }
}
\value{
  \code{allDigits} The strings contain digits only which are allowable in base 'base'.\cr
  \code{isNumeric} Test whether the elements of a character vector
	represent legal numbers only.\cr
  \code{str2dig} Convert a string to a vector of integers.\cr
}
\examples{
  allDigits(c("1231","89a8742")) #  TRUE FALSE
  isNumeric(c("1231","8.9e-2",".7d2")) # [1]  TRUE  TRUE FALSE
  str2dig("13245.") #  1 3 2 4 5 NA
    # for comparison, big numbers:
  int(10^(7:10)) # 10000000 100000000 1000000000 NA
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
