\name{formula2string}
\alias{formula2string}
\alias{formula2term.names}
\alias{formula2Rterm.names}
\title{Extract sides of a formula to strings}
\description{
  \code{formula2string} Returns the left and the right hand sides of a
	formula.
	
  \code{formula2term.names} Returns one chosen side of a
	formula.
	
  \code{formula2Rterm.names} Returns the right hand side of a
	formula.
}
\usage{
formula2string(form)
formula2term.names(form,side)
formula2Rterm.names(form)
}
\arguments{
  \item{form}{a formula.}
  \item{side}{one of "left","right".}
}
\value{
  \code{formula2string}: Character vector containing the string
	representation of the formulas side(s).
}
\examples{
formula2string(a ~ b + c)  #  $left: "a"   $right: "b+c"
formula2string(~ b + c)    #  $left: ""    $right: "b+c"
formula2string(a ~ .)      #  $left: "a"   $right: "."
formula2term.names(a ~ b1 + c,"left")  #   "a"
formula2Rterm.names(a ~ b1 + c)        #   "b1" "c"
}
\seealso{
  The inverse function to \code{formula2string} is \code{\link{str2formula}}.
}
\note{
  uses \code{strsplit}
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{character}
\keyword{manip}
