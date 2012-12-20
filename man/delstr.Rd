\name{delstr}
\alias{delstr}
\title{Delete a substring from a string}
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
delstr("Ich gehen heuten in den Garten hinein","en")
#  "Ich geh heut in d Gart hinein"
delstr("12345","x") # "12345" 
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{print}
