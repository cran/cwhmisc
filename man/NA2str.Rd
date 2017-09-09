\name{NA2str}
\alias{NA2str}
\title{Convert NA, NaN, Inf to a string}
\description{
 Conversion of indefinite values
}
\usage{
  NA2str( x )
}
\arguments{
  \item{x}{A numerical vector.}
}
\examples{
 NA2str(c(NA,NaN) ) # "NA"  "NaN"
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{print}
