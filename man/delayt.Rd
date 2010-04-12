\name{delayt}
\alias{delayt}
\title{Waiting loop for program execution}
\description{
  Wait for approximately \code{sec} seconds during program execution
}
\usage{
  delayt(sec) # wait for sec seconds
}
\arguments{
  \item{sec}{Number of seconds to wait}
}
\details{
  calls Sys.time()
}
\value{
  the number of internal calls of Sys.time()
	}
\examples{
  Sys.time(); nrof <- delayt(5); Sys.time()
  print(nrof) # 2620 on my machine
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{chron}

