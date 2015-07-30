\name{grepno}
\alias{grepnot}
\title{Grep utility}
\description{
 Show elements passing or not a grep.
}
\usage{
grepnot(s,x,value=TRUE)
}
\arguments{
  \item{s,x}{String}
  \item{value}{Boolean}
}
\details{
  \code{s} String regular expression, see \code{link{grep}}.\cr
  \code{x} Array of strings to check with \code{grep}.\cr
  \code{value} Third argument to grep proper.\cr
  (strings which TRUE) "\\" (strings with FALSE), "\\" stands for "NOT"
}
\value{
  Elements 
}
\examples{
x <- c("Ab"," d","#12","  ",".1", "m.","",  "$y")
y <- c(".x", " .","c.",  "$y", "_p","12")
grepnot("^A[[:alpha:]]+",x) #  "Ab"  "\\"  " d"  "#12" "  "  ".1"  "m."  ""    "$y"
grepnot("A[[:alpha:]]+",x)  #  "Ab"  "\\"  " d"  "#12" "  "  ".1"  "m."  ""    "$y"
grepnot("a[[:alpha:]]+",x) # "\\" ...
grepnot("#[[:alnum:]]+",x) #  "#12" "\\" ...
grepnot("$",x) #  ""    "\\" ...
grepnot("[^$][[:alnum:]].]+",x) # "\\" ...
grepnot("[^[$]][[:alnum:]].]+",x) # "\\" ...
grepnot("^.[[:alpha:]]+",x,FALSE) # "1"   "2"   "\\" "3"   "4"   "5"   "6"   "7" 
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character}
