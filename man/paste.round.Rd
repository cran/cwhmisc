\name{pasteRound}
\alias{pasteRound}
\title{Paste rounded values}
\description{
  Paste rounded values
}
\usage{
pasteRound(..., digits=16, sep=" ", collapse=NULL)
}
\arguments{
  \item{\dots}{list of arguments to be pasted.}
  \item{digits}{Integer, argument to \code{\link{round}}.}
  \item{sep, collapse}{Character, arguments to \code{\link{paste}}.}
}
\value{
The concatenation of formatted values
}
\examples{
x <- rnorm(3)
x
matrix(pasteRound("x1=", x[1], ", x2=", x[2], ", x3=", x[3], sep="",
 collapse=","),ncol=1)
matrix(pasteRound("x1=", x[1], ", x2=", x[2], ", x3=", x[3], digits=3,
 sep="$", collapse="_"),ncol=1)
}
\author{
  Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.ac.be},
 adapted by Christian Hoffmann \email{christian@echoffmann.ch}
}
\keyword{misc}
\keyword{documentation}
