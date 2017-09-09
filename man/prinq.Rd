\name{printP}
\alias{catn}
\alias{catE}
\alias{prinE}
\alias{prinP}
\alias{prinV}
\alias{prinM}
\alias{prinT}
\alias{NprinE}
\alias{NprinP}
\alias{NprinV}
\alias{NprinM}
\alias{NprinT}
\title{Print without square brackets, expression values together with their call strings}
\description{
  These functions may be helpful for documenting ongoing work using \code{sink()}. \cr
}
\usage{
  catn(...)
  catE(...)
  prinE(...,digits=4)
  prinV(x,after=2,before)
  prinM(x,after=2,before)
  prinT(x,rownam=FALSE,colnam=FALSE)
  prinP(xs)
}
\arguments{
  \item{\dots}{See 'note'.}
  \item{x}{A numerical vector or matrix.}
  \item{digits}{Integer, number of digits, see \code{\link{print}} }
  \item{before}{Integer, the number of decimals before "."}
  \item{after}{Integer, the number of decimals after ".".}
  \item{rownam}{Logical, should row names be printed.}
  \item{colnam}{Logical, should column names be printed.}
  \item{xs}{A string representing an expression.}
}
\note{
\code{-catn()}    is shorthand for cat("\\n") which is awkward for me to type.\cr
\code{-catE,prinE}   print string expressions \dots and their
evaluation in the form "expression = (newline) evaluation", in vector
form.\cr
\code{-catE} is like 'prinE', but can handle annotating (non-variable)
strings, given as starting with '\\t'. If line feed is wanted, start with '\\n'. It *cannot* handle matrices.\cr
\code{-prinP}   prints a string argument and evaluates it i.e. the body of the function evaluated should contain print and cat statements. \cr
\code{-prinV}   prints a vector without [], in fix format. \cr
\code{-prinM}   prints a matrix without [], in fix format. \cr
\code{-prinT}   prints an array, TAB delimited. \cr
The variants N\ldots prepend a linefeed.\cr
}
\seealso{
  \code{\link{formatFix}}
}
\examples{
xx <- options(digits=7)
x <- matrix(c(5,3,2,7,8.235,exp(1),pi,0,99),3,3)
m <- matrix(c("a","b c","d","ff"," x","","7","8","99"),3,3)
dimnames(x) <- list(c("r1","r2","r3"),c("c1","c2","c3"))


prinV(as.vector(x))
#  5.00  3.00  2.00  7.00  8.24  2.72  3.14  0.00 99.00

prinM(x,,3)
#  5.00  7.00  3.14
#  3.00  8.24  0.00 
#  2.00  2.72 99.00 

prinT(x,TRUE,TRUE)
#   	c1	c2	c3   OK
# r1	5	7	3.14159265358979 
# r2	3	8.235	0 
# r3	2	2.71828182845905	99 

prinT(c(c1="a",c2="b c",c3="d",c4="ff",c5=" x"),TRUE)
# c1	c2	c3	c4	c5 
# a	b c	d	ff	 x

prinT(c(c1=5,c2=7,c3=1,c4=3),TRUE)
# 5	7	1	3
 opt <- options(digits=3)
prinE("x")
prinE("'This is a comment: ';3+5;pi-3",digits=4)
prinE("x")
# x =	   c1    c2     c3
# r1  5 7.000  3.142
# r2  3 8.235  0.000
# r3  2 2.718 99.000


catt <- function(x) {cat(paste0("This function 'catt' will write '",x,"' on one line\n")) }
y <- prinP("catt(32)");
# catt(32) 
# This function will write ' 32 ' on one line

prinE("y ")
# y =[1] "catt(32)"

prinE("y ")
# y  =	NULL

prinP("y ")
# y 
 
options(digits=xx$digits)
}

\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{print}
