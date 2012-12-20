\name{RCA}
\alias{RCA}
\alias{RC}
\title{
  Check, build, install package.
}
\description{
  RC: Use with type = ("c"), "b", "c", "a", "i", in that order.\cr
  RCA: Execute RC with any of 0:5, typically 2:5. Check for errors and repeat. Package treated is located in directory paste(dir,"/",pkg,sep="")
}
\usage{
  RC(dir,pkg,type="c")
  RCA(dir,pkg,trig=c(0,2:5))
}
\arguments{
  \item{dir}{character, path of (collection of) package(s)}
  \item{pkg}{character, name of package}
  \item{type}{"a", "b", "c","i" for 'check --as-cran', 'build', 'check','install'}
  \item{trig}{integer, any subset of 1:5 corresponding to "c", "b", "c", "a", "i" in RC, 0 for an explanatory text concerning "c"}
}
\examples{
\dontrun{
  RC("/User/.../R/sources","pack","c")  # treating "/User/.../R/sources/pack"
  RCA("...", 2:4) # corresponding to RC(...,"b"); RC(...,"c"); RC(...,"i")
}
}
\note{
  "RC" calls system("R CMD <options> path-to-package ") with options "check --as-cran","build","check","install" corresponding to "a","b","c","i", triggered by "trig" 4, 2, 1 and 3, 5, respectively. The order 1 to 5 is suggested by 'Writing extensions'. For trig = 0 a message is printed:  For checking as required by CRAN policy, run RC() with pkg='pkg_VERSION.tar.gz'.
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{utils}
