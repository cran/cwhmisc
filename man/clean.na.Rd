\name{clean.na}
\alias{clean.na}
\title{Clean a matrix or data frame of rows or columns of containing NA.}
\description{
  \code{clean.na} Eliminate rows or columns containing NA.
}
\usage{
clean.na(x,margin,drop=FALSE)
}
\arguments{
  \item{x}{A matrix.}
  \item{margin}{= 1 for rows, = 2 for columns}
  \item{drop}{= FALSE (default) if result should be a matrix even if it
		contains only one row or column.}
}
\value{
	The matrix without the offending rows or columns.
}
\seealso{
  \code{\link{drop}}.
}
\examples{
  x <- matrix(c(1,NA,2,5),2,2)
  clean.na(x,1)
#     [,1] [,2]
#[1,]    1    2
  clean.na(x,2,TRUE)
# [1] 2 5
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{manip}
