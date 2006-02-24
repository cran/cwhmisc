\name{mathlib-internal}
\alias{adaptsimstp}
\alias{adaptlobstp}
\title{Internal mathlib functions}
\description{
 Internal mathlib functions
}
\usage{
adaptsimstp(f,a,b,fa,fm,fb,is,trace,...) 
adaptlobstp(f,a,b,fa,fb,is,trace,...)}
\arguments{
  \item{f}{function to be integrated.}
  \item{a}{starting abscissa of integral.}
  \item{b}{ending abscissa of integral.}
  \item{fa}{=f(a).}
  \item{fb}{=f(b).}
  \item{fm}{=f((a+b)/2).}
  \item{is}{.}
  \item{trace}{if TRUE then trace will be displayed.}
  \item{...}{additional coefficients for function  \code{f}  if necessary.}
}
\value{
  The approximate value of the integral. 
}
\details{
  These functions are not to be called by the user.
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch}, \cr
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{documentation}
