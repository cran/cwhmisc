\name{my.table}
\alias{my.table.NA}
\alias{my.table.margin}
\title{Tabulate data, with extra rows and columns.}
\description{
  \code{my.table.NA} tabulates a vector of values and lists NA and NaN at the
  beginning, if they occur.
  \code{my.table.margin} generates contingency table together with both margins of two factors, or of a matrix, if only one parameter is given.
}
\usage{
my.table.NA(x, relative=FALSE)
my.table.margin(v,w)
}
\arguments{
  \item{x}{A vector, will be converted to factors.}
  \item{relative}{= TRUE if relative values should be returned.}
  \item{v}{factor or matrix.}
  \item{w}{factor.}
}
\value{
	A contingency table.
}
\note{
	Uses \code{\link{table}}.
}
\examples{
  x <- c(1,NA,2,5,-1:7)
  my.table.NA(x)
  f1 <- sample(1:5,100,replace=TRUE) 
  f2 <- sample(1:5,100,replace=TRUE)
  my.table.margin(f1,f2)
  my.table.margin(matrix(1:24,4))
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch> \cr
  and   John Fox \email{jfox@mcmaster.ca} (my.table.margin)
}
\keyword{distribution}
