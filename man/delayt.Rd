\name{delayt}
\alias{delayt}
\title{Waiting loop for program execution}
\description{
  Wait for approximately \code{sec} seconds during program execution
}
\usage{
  delayt(sec) # wait for sec seconds
  nrof <- delayt(sec) # save the number of internal calls of Sys.time(). Can be used to compare the relative execution speed of different processors.
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
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{chron}

