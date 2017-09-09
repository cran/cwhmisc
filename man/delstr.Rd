\name{delstr}
\alias{delstr}
\title{String handling}
\description{
  \code{delstr} deletes a substring from a string
}
\usage{
delstr(str,del)
}
\arguments{
  \item{str}{a string, may be empty, string to be edited}
  \item{del}{a string, may be empty, string to be taken out.}
}
\value{
 A string
}
\examples{
delstr("Don't enter my garden","en")
# -> "Don't ter my gard"
delstr("12345","2") # "1345"

strReverse(c("abc", "Statistics")) #  "cba" "scitsitatS"
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{print}
