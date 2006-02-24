\name{dt2str}
\alias{dt2str}
\title{ Convert time difference to string. }
\description{Convert time difference to string depending on switch.
}
\usage{
  dt2str(dt,verbose=FALSE)
}
\arguments{
  \item{dt}{Time difference in seconds}
  \item{verbose}{If \code{TRUE}, then "hours minutes seconds", else "::"}
}
\value{
  String representing the time difference.
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\examples{
  time1 <- Sys.time()
  x <- 0
  for (i in 1:100000) x <- x+1
  time2 <- Sys.time()
  dt2str(unclass(time2)-unclass(time1))
  dt2str(unclass(time2)-unclass(time1),TRUE)
}
\keyword{chron}

