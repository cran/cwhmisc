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
  \item{digits}{argument to \code{\link{round}}.}
  \item{sep}{argument to \code{\link{paste}}.}
  \item{collapse}{argument to \code{\link{paste}}.}
}
\value{
The concatenation of formatted values
}
\examples{
x <- rnorm(3)
x
pasteRound("x1=", x[1], ", x2=", x[2], ", x3=", x[3], sep="", collapse=",")
pasteRound("x1=", x[1], ", x2=", x[2], ", x3=", x[3], digits=3, sep="", collapse=",")
}
\author{
  Dimitris Rizopoulos \email{dimitris.rizopoulos@med.kuleuven.ac.be}
}
\keyword{misc}
\keyword{documentation}
