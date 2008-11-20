\name{whole.number}
\alias{whole.number}
\title{Check an array on whole numbers (x in I).}
\description{
  \code{whole.number} checks an array whether it consists of whole numbers only (x in I).
}
\usage{
whole.number(x)
}
\arguments{
  \item{x}{A numerical array.}
}
\value{
	\code{\link{TRUE}}, \code{\link{FALSE}}
}
\examples{
whole.number(c(pi,2,3)) # FALSE
whole.number(c(1,2,3))  # TRUE
}
\author{Bill Venables
adapted by   Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN?-C=&n}
}
\keyword{data}

