\name{waitReturn}
\alias{waitReturn}
\title{Wait for <Return>}
\description{
  Wait for the user to type <Return>, depending on argument.
}
\usage{
  waitReturn(q="",ask=TRUE)
}
\arguments{
  \item{ask}{ \code{TRUE} will generate the interruption, \code{FALSE} will not.}
  \item{q}{ String for prompt}
}
\details{
  The interruption will only be generated for the interactive use of R and
  if the call is not \code{sink}ed (where it would hang the process).
}
\value{
  None.
}

\examples{
  for (ii in 1:5) {
    cat(ii,"\n")
    waitReturn(ii \%\% 2 == 1) 
  } 
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{programming}

