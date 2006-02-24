\name{str2formula}
\alias{str2formula}
\alias{term.names2formula}
\title{Convert back to a formula}
\description{
  \code{str2formula} is the inverse function to \code{formula2string}.
  \code{term.names2formula} combines two vectors of strings into a formula.
}
\usage{
  str2formula(s)
  term.names2formula(ls,rs)
}
\arguments{
  \item{s}{A \code{list(left,right)} containing the string representation of
		the left and the right hand side of the formula (one string each).}
  \item{ls}{A character vector (usually of length 1) containing the
		names of the terms on the left hand side of the formula.}	
  \item{rs}{A character vector containing the names of the terms on the right hand side of the formula.}	
}
\value{
	A formula.
}
\seealso{
  The inverse function to \code{str2formula} is \code{\link{formula2string}}.
  The inverse functions to \code{term.names2formula} are
	\code{\link{formula2term.names}} and \code{\link{formula2Rterm.names}}.
}
\examples{
fo <- a ~ b + c
abc <- formula2string(fo)  #  $left: "a"   $right: "b+c"
str2formula(abc)    # a ~ b + c
term.names2formula(formula2term.names(fo,"left"),c(formula2Rterm.names(fo),"X")) 
 # a ~ b + c + X
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{character}
\keyword{manip}
