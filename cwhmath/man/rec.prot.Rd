\name{rec.prot}
\alias{rec.prot}
\title{Protected division}
\description{
  1/x, but 1/0 -> 0
}
\usage{
  rec.prot(x, eps = .Machine$double.eps)
}
\arguments{
  \item{x}{array to invert.}
  \item{eps}{limit: if abs(x) < eps then 1/x -> 0.}
}
\examples{
  rec.prot(c(0,.Machine$double.eps,1.0e-10,1,1.0e100))
  rec.prot(c(0,.Machine$double.eps,1.0e-10,1,1.0e100), 100*.Machine$double.eps)
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{algebra}
