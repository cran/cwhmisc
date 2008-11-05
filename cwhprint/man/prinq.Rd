\name{printP}
\alias{printP}
\alias{catn}
\alias{pn}
\alias{prinV}
\alias{prinM}
\alias{prinT}
\alias{prinE}
\alias{prinL}
\alias{prinP}
\alias{NprinV}
\alias{NprinM}
\alias{NprinT}
\alias{NprinE}
\alias{NprinL}
\alias{NprinP}
\title{Print without square brackets, expression values together with their call strings}
\description{
  These functions may be helpful for documenting ongoing work using \code{\sink()}. \cr
-  \code{catn(\dots)} is shorthand for cat(\dots); cat("\\n") \cr
-  \code{pn()}    is shorthand for cat("\\n") which is awkward for me to type. \cr
-  \code{prinV}   prints a vector without \[\], in fix format. \cr
-  \code{prinM}   prints a matrix without \[\], in fix format. \cr
-  \code{prinT}   prints an array, TAB delimited. \cr
-  \code{prinE(xsv, \dots)}   prints a string expression and its evaluation in the form "xsv = evaluation", in vector form. The string may contain "'commenting text'; expression(s)", but only the last expression will be evaluated. \cr
-  \code{prinL(xs, \dots)}    prints a string expression and its evaluation in the form "xs \\newline evaluation". \cr
-  \code{prinP(xs)}   prints a string argument and evaluates it i.e. the body of the function evaluated should contain print and cat statements. \cr
--  The variants N\ldots prepend a linefeed. \cr
}
\usage{
  catn(...)
  pn()
  prinV(x,after=2,before)
  prinM(x,after=2,before)
  prinT(x,rownam=FALSE,colnam=FALSE)
  prinE(xsv,...)
  prinL(xs,...)
  prinP(xs)
}
\arguments{
  \item{x}{A numerical vector or matrix.}
  \item{before}{See \code{\link[cwhstring]{formatFix}}, the number of decimals before "."}
  \item{after}{See \code{\link[cwhstring]{formatFix}}, the number of decimals after ".".}
  \item{rownam}{Should row names be printed.}
  \item{colnam}{Should column names be printed.}
  \item{xsv}{A string representing a vector expression.}
  \item{xs}{A string representing an expression.}
  \item{\dots}{Additional parameters for \code{\link{print}}.}
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
#   	c1	c2	c3 
# r1	5	7	3.14159265358979 
# r2	3	8.235	0 
# r3	2	2.71828182845905	99 

prinT(c(c1="a",c2="b c",c3="d",c4="ff",c5=" x"),TRUE)
# c1	c2	c3	c4	c5 
# a	b c	d	ff	 x      # the tabs are not visible here

prinT(c(c1=5,c2=7,c3=1,c4=3),TRUE)
# 5	7	1	3      # the tabs are not visible here

####prinE("x")
# x =[1]  5.000000  3.000000  2.000000  7.000000  8.235000  2.718282  3.141593
# [8]  0.000000 99.000000

####prinE("'This is a comment: ';3+5;pi-3",digits=4)
# 'This is a comment: ';3+5;pi-3 =[1] 0.1416

prinL("x")
# x 
#  	 c1       c2        c3
# r1  5 7.000000  3.141593
# r2  3 8.235000  0.000000
# r3  2 2.718282 99.000000

catt <- function(x) {cat("This function will write '",x,"' on one line\n") }
y <- prinP("catt(32)");
# catt(32) 
# This function will write ' 32 ' on one line

####prinE("y ")
# y =[1] "catt(32)"

prinL("y ")
# y  
# [1] "catt(32)"

prinP("y ")
# y 
 
options(digits=xx$digits)
}

\note{
 Uses \code{\link[cwhstring]{formatFix}} in package "cwhstring"
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{print}
