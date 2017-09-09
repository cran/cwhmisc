\name{RCA}
\alias{RCA}
\title{
  Check, build, install package in a unified manner.
}
\description{
Check, build, install package in a unified manner.
}
\usage{
  RCA(dir=getwd(),pkg, Rsty, sw=c(2, 5:7), echoonly=FALSE, verbose=TRUE)
}
\arguments{
  \item{dir}{character, code{link{dirname}} of package(s).}
  \item{pkg}{character, code{link{basename}} of package}
  \item{Rsty}{full path name of 'Rd.sty'}
  \item{echoonly}{boolean, give echo of R CMD ..., more verbosely}
  \item{verbose}{boolean, give only echo of intended 'R CMD ...'}
  \item{sw}{switch, for alternatives, must be in 0:6, see note}
}
\examples{
  \dontrun{
  RCA(,"mypackage", 2:4)
  }
}
\note{
  If the complete filepath of the package source is given in 'dir', 'pkg' must be empty!\cr\cr
  "RCA" calls system("R CMD <options> path-to-package ") with options\cr
  \code{sw}: \cr 
   - 0  = (show sw alternatives), \cr
   - 1  = "Rd2pdf --no-clean --force", \cr
   - 2  = "check", \cr
   - 3  = "build --force --no-build-vignettes", \cr
   - 4  = "check --as-cran <pkg>.tar.gz", \cr
   - 5  = "check --as-cran", \cr
   - 6  = "install" \cr
   - 7  = "Sweave ", "/vignettes/",".Rnw" \cr
The order 2 to 6 is suggested by \url{https://cran.r-project.org/doc/manuals/r-release/R-exts.pdf}. \cr
sw = 1 shows errors present in the creation of the manual. \cr
sw = 4 is provided for checking as required by CRAN policy. \cr
\emph{'Rd.sty'} must be provided in \code{Rsty}. \cr
Permissions for the vignette *.Rnw should be changed by system("chmod
u=rwx ...Rnw "), if necessary.
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{utilities}
