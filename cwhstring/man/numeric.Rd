\name{numeric.string}
\alias{numeric.string}
\alias{all.digits}
\title{Test string for being a number or made up of digits only.}
\description{
  \code{numeric.string} Test whether the elements of a character vector
	represent legal numbers only.
  \code{all.digits} Test whether the elements of a character vector consist of digits only
}
\usage{
numeric.string(str)
all.digits(str)
}
\arguments{
  \item{str}{A character vector.}
}
\value{
	TRUE, FALSE
}
\examples{
all.digits(c("1231","89a8742")) #  TRUE FALSE
numeric.string(c("1231","8.9e-2",".7d2")) # [1]  TRUE  TRUE FALSE
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{character}
\keyword{arith}

