\name{RCA}
\alias{RCA}
\title{
  Check, build, install package in a unified manner.
}
\description{
Check, build, install package in a unified manner.
}
\usage{
  RCA(dir,pkg,sw=0:6, echoonly=FALSE, verbose=TRUE)
}
\arguments{
  \item{dir}{character, path of (collection of) package(s)}
  \item{pkg}{character, name of package}
  \item{echoonly}{boolean give echo of R CMD ..., more verbosely}
  \item{verbose}{boolean give only echo of intended 'R CMD ...'}
  \item{sw}{switch for alternatives, must be in 0:6, see note}
}
\examples{
\dontrun{
  RCA(<R-dir>,"mypackage", 2:4)
}
}
\note{
  "RCA" calls system("R CMD <options> path-to-package ") with options \code{sw} \cr 
   0  = (show sw alternatives), \cr
   1  = "Rd2pdf --no-clean --force", \cr
   2  = "check", \cr
   3  = "build --force", \cr
   4  = "check --as-cran <pkg>.tar.gz", \cr
   5  = "check --as-cran", \cr
   6  = "install". \cr
The order 2 to 6 is suggested by 'Writing extensions'. \cr
sw = 1 shows errors present in the creation of the manual. \cr
sw = 4 is provided for checking as required by CRAN policy. \cr
 'Rd.sty' should be provided in the directory where RCA is run. \cr
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{utils}
