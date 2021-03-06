\name{n2c}
\alias{n2c}
\alias{indexLine}
\alias{n2cCompact}
\alias{charMat}
\alias{explainLegend}
\title{Show absolute values as characters, prepare for plotting}
\description{
  \code{n2c} takes a numerical vector or matrix and represents it as
	single characters, with attribute \code{legend}. 
  \code{indexLine} generates a string with dots, ";", and digits, usable
  as x-label in \code{n2cCompact}: .....;....1....;....2..
  \code{n2cCompact} combines \code{n2c} and \code{indexLine} to generate
  a vector of strings good for printing numerical matrices. 
  \code{charMat} processes the output from \code{n2cCompact} and returns
  vectors x, y, tx of equal lengths for input to \code{\link{pltCharMat}}. 
  \code{explainLegend} gives a more readable version of attribute \code{legend}.
}
\usage{
  n2c(x, symm = FALSE)
  indexLine(n)
  n2cCompact(x, symm=FALSE)
  charMat(cc)
  explainLegend()
}
\arguments{
  \item{x}{A numerical vector or matrix.}
  \item{symm}{If \code{symm = TRUE} then upper triangle will be
	suppressed.}
  \item{n}{integer, length of string wanted}
  \item{cc}{ output from \code{n2cCompact}, input to \code{charMat}}
}
\value{
  \code{n2c} Representation of \code{x} as a single-character matrix, as
  explained in \emph{attribute} \code{legend}.
  \code{n2cCompact} pack 
  \code{charMat} list(x,y,txt)
}
\note{
	Empty places due to \code{symm = TRUE} are filled with " ".
}
\examples{
n2c(c(10e20,-10e5,10,(10:0)/10,0.05)) 
#   "X" "6" "1" "0" "&" "%" "#" "*" "=" "+" "-" ":" "," " "
# attr(,"legend")
# [1] ">=1:log, >=0. 9& 8% 7# 6* 5= 4+ 3- 2: 1, 05. ' ' "

n2c(matrix(c(10e20,10e5,20,10,0.7,0.6,0,0.5,0.1),3,3),FALSE)
#      [,1] [,2] [,3]
# [1,] "X"  "1"  " " 
# [2,] "5"  "#"  "=" 
# [3,] "1"  "*"  "," 
# attr(,"legend")
# [1] ">=1: log, >=0. 9& 8\% 7# 6* 5= 4+ 3- 2: 1, 05. ' ' "
m <- matrix(rnorm(500),nrow=50,ncol=10)
n2c(m,symm=TRUE)
indexLine(ncol(m))
(n2 <- n2cCompact(m, symm=FALSE))
charMat(n2)
explainLegend() # 
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{print}
