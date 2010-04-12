\name{div.prot}
\alias{div.prot}
\title{Protected division}
\description{
  1/x, but 1/0 -> 0
}
\usage{
  div.prot(x, eps = .Machine$double.eps)
}
\arguments{
  \item{x}{array to invert.}
  \item{eps}{limit: if abs(x) < eps then 1/x -> 0.}
}
\examples{
  div.prot(c(0,.Machine$double.eps,1.0e-10,1,1.0e100))
  div.prot(c(0,.Machine$double.eps,1.0e-10,1,1.0e100), 100*.Machine$double.eps)
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{algebra}
