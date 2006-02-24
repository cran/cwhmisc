\name{cbind.colnames}
\alias{cbind.colnames}
\title{Add columns to a data frame, using variable names.}
\description{
  \code{cbind.colnames} adds columns to a given data frame. The names of the 
	variables to be added are given in character vector \code{add}.
}
\usage{
cbind.colnames(add,to=NULL,deparse.level = 1)
}
\arguments{
  \item{to}{Matrix to be augmented.}
  \item{add}{Vector of names of variables to be added to \code{to} as
		columns.}
  \item{deparse.level}{Usually 1, See \code{\link{cbind}}}
}
\value{
  Combined matrix. The column names are the concatenation of those of
	\code{to}  and \code{add}. Row names are taken from \code{to}.
}
\note{
	Uses \code{get} to access the variables listed in \code{add}.
	If the new variables have wrong lenghts, an error will result.
}
\examples{
d  <- data.frame(E = c("D","at","a"),w = c(11,22,33))
x  <- c(2,5,1)
ch <- c("I","have","fun")
F  <- factor(c("A","B","C"))
cbind.colnames(c("F","ch","x","F"),d)
#    E  w F   ch x F
# 1  D 11 A    I 2 A
# 2 at 22 B have 5 B
# 3  a 33 C  fun 1 C
cbind.colnames(c("x","F","ch","x"))
data.frame(x,F,ch,x) # the same
cbind.colnames(NULL) # NULL
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{manip}

