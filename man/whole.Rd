\name{whole}
\alias{whole}
\title{Check an array on whole numbers (x in I).}
\description{
  \code{whole} checks an array whether it consists of whole, i.e. integer , numbers only (x in I).
}
\usage{
whole(x)
}
\arguments{
  \item{x}{A numerical array.}
}
\value{
	\code{\link{TRUE}}, \code{\link{FALSE}}
}
\examples{
whole(c(pi,2,3)) # FALSE
whole(c(1,2,3))  # TRUE
}
\author{Bill Venables
adapted by   Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{data}

