\name{n22dig}
\alias{n22dig}
\title{Show vector or matrix (of 0 <= x <=1) in a compact way
}
\description{
  \code{n22dig} shows	"0.ab" as "ab", "1.00" as " I", "0" as " 0".
}
\usage{
n22dig(x, symm = TRUE)
}
\arguments{
  \item{x}{A numerical vector or matrix.}
  \item{symm}{If \code{symm = TRUE} then upper triangle will be suppressed.}
}
\value{
	Representation of \code{x} as two-digit vector or matrix.
}
\note{
  A violation of the condition on abs(x) will not be signalled.
	Empty places due to \code{symm = TRUE} are filled with " ".
}
\seealso{
  \code{\link{n2c}}.
}
\examples{
n22dig(cor(matrix(rnorm(100),10)),TRUE)
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#  [1,] " I" " "  " "  " "  " "  " "  " "  " "  " "  " "  
#  [2,] "10" " I" " "  " "  " "  " "  " "  " "  " "  " "  
#  [3,] " 8" "26" " I" " "  " "  " "  " "  " "  " "  " "  
#  [4,] " 8" "49" " 2" " I" " "  " "  " "  " "  " "  " "  
#  [5,] " 8" "22" " 9" "46" " I" " "  " "  " "  " "  " "  
#  [6,] "40" "26" " 5" "27" "14" " I" " "  " "  " "  " "  
#  [7,] " 8" "15" "21" "58" "13" "26" " I" " "  " "  " "  
#  [8,] "13" "30" " 2" "58" "21" "41" "61" " I" " "  " "  
#  [9,] "46" "22" " 7" "63" "15" "25" "43" "36" " I" " "  
# [10,] "66" "51" "48" "16" "20" "27" "28" "20" "16" " I" 
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{print}
