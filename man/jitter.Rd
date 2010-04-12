\name{jitterNA}  % only alphanumeric
\alias{jitterNA}
\title{Jitter entities with NA}
\description{
  Extension of \code{\link[base]{jitter}} to deal with NA entries
}
\usage{
  jitterNA(x,...)
}
\arguments{
  \item{x}{Data to be jittered, may be vector, matrix, or numerical data frame.}
  \item{\dots}{Other parameters for \code{\link[base]{jitter}}.}
}
\value{
  jitterNA(x, ...) return a numeric vector with jittered entries, NA
  entries are allowed and not changed
}
\examples{
  d <- data.frame(cbind(x=1, y=1:10))
  d[5,1] <- d[3,2] <- NA
  jitterNA(d)
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{data}
\keyword{NA}
