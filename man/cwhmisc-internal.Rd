\name{cwhmisc-internal}
\alias{ASCII}
\alias{HexDig}
\alias{spacC}
\alias{caplow.ff}
\alias{adaptsimstp}
\alias{adaptlobstp}
\alias{trigrid}
\title{Internal cwhmisc functions}
\description{
 Internal cwhmisc functions
}
\usage{
adaptsimstp(f,a,b,fa,fm,fb,is,trace,...) 
adaptlobstp(f,a,b,fa,fb,is,trace,...)
trigrid(grid = FALSE)
}
\arguments{
  \item{f}{function to be integrated.}
  \item{a}{starting abscissa of integral.}
  \item{b}{ending abscissa of integral.}
  \item{fa}{=f(a).}
  \item{fb}{=f(b).}
  \item{fm}{=f((a+b)/2).}
  \item{is}{used for stopping at a suitable multiple of the machine precision.}
  \item{trace}{if TRUE then trace will be displayed.}
  \item{...}{additional coefficients for function  \code{f}  if necessary.}
  \item{grid}{Plot the grid: TRUE or FALSE}
}
\value{
  The approximate value of the integral. 
}
\details{
  These functions are not to be called by the user.
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{documentation}
