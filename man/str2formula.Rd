\name{str2formula}
\alias{str2formula}
\alias{term.names2formula}
\alias{formula2string}
\alias{formula2term.names}
\alias{formula2Rterm.names}
\title{Extract sides of a formula to strings, Convert back to a formula}
\description{
  \code{str2formula} is the inverse function to \code{formula2string}.
  \code{term.names2formula} combines two vectors of strings into a formula.
  \code{formula2string} Returns the left and the right hand sides of a
	formula.
	
  \code{formula2term.names} Returns one chosen side of a
	formula.
	
  \code{formula2Rterm.names} Returns the right hand side of a
	formula.
}
\usage{
  str2formula(s)
  term.names2formula(ls,rs)
  formula2string(form)
  formula2term.names(form,side)
  formula2Rterm.names(form)
}
\arguments{
  \item{s}{A \code{list(left,right)} containing the string representation of
		the left and the right hand side of the formula (one string each).}
  \item{ls}{A character vector (usually of length 1) containing the
		names of the terms on the left hand side of the formula.}	
  \item{rs}{A character vector containing the names of the terms on the right hand side of the formula.}	
  \item{form}{a formula.}
  \item{side}{one of "left","right".}
}
\value{
  \code{str2formula}:	A formula.
  \code{formula2string}: Character vector containing the string
	representation of the formulas side(s).
}
\note{
  The inverse function to \code{str2formula} is \code{\link{formula2string}}.\cr
  The inverse functions to \code{term.names2formula} are
	\code{\link{formula2term.names}} and \code{\link{formula2Rterm.names}}.\cr
  Functions use \code{strsplit}
}
\examples{
fo <- a ~ b + c
abc <- formula2string(fo)  #  $left: "a"   $right: "b+c"
str2formula(abc)    # a ~ b + c
term.names2formula(formula2term.names(fo,"left"),c(formula2Rterm.names(fo),"X"))  # a ~ b + c + X
formula2string(a ~ b + c)  #  $left: "a"   $right: "b+c"
formula2string(~ b + c)    #  $left: ""    $right: "b+c"
formula2string(a ~ .)      #  $left: "a"   $right: "."
formula2term.names(a ~ b1 + c,"left")  #   "a"
formula2Rterm.names(a ~ b1 + c)        #   "b1" "c"
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{character}
\keyword{manip}
