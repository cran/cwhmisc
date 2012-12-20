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
  String representing the time difference, with \code{dec} decimals in seconds.
}
\examples{
  t1 <- unclass(Sys.time())
  x <- 0;  for (i in 1:1.e6) x <- x+1
  t2 <- unclass(Sys.time())
  dt2str(t2-t1,3) # 00:00:2.835
}
\note{
  Enclosing the above statements in a function is likely to show zero time.
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{chron}

