\name{normalize}
\alias{normalize}
\title{Normalize vectors}
\description{
  Normalize the columns of a matrix.
}
\usage{
  normalize(x)
}
\arguments{
  \item{x}{the matrix containing the vectors to be normalized as \emph{columns}.}
}
\value{
  The matrix containing the normalized vectors as \emph{columns}. If you
  want to normalize the \emph{rows} of a matrix, use  \code{t(normalize(t(x)))}
}
\examples{
  x <- matrix(rnorm(30),nrow=6)
  normalize(x)
  t(normalize(t(x)))  # normalize the *rows* of x
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{algebra}
