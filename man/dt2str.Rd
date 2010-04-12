\name{dt2str}
\alias{dt2str}
\title{ Convert time difference to string. }
\description{Convert time difference in seconds to string depending on switch.
}
\usage{
  dt2str(dt,dec=0,verbose=FALSE)
}
\arguments{
  \item{dt}{Time difference in seconds}
  \item{dec}{Places in decimal fraction of seconds}
  \item{verbose}{If \code{TRUE}, then delimited by "hours minutes seconds", else by ":"}
}
\value{
  String representing the time difference.
}
\examples{
  time1 <- Sys.time()
  x <- 0
  for (i in 1:100000) x <- x+1
  time2 <- Sys.time()
  dt2str(unclass(time2)-unclass(time1))
  dt2str(unclass(time2)-unclass(time1),,TRUE)
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{chron}

