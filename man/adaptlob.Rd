\name{adaptlob}
\alias{adaptlob}
\alias{adaptsim}
\title{Numerically evaluate integral using adaptive rule.}
\description{
  \code{adaptsim} and \code{adaptlob} approximate the integral of the
	function \code{f} using \emph{adaptive} Simpson and Lobatto rule.
	Both methods can deal with discontinuous functions.
	
	\code{adaptlob} is more efficient than \code{adaptsim} when the accuracy requirement 
	is high. For lower tolerances, \code{adaptsim} is generally (but not
	always) more efficient than \code{adaptlob}, but less reliable.

	Both routines show excellent response to changes in the tolerance.
	
  The function \code{f} must return a vector of output values if given a
	vector of input values.

	\code{adapt...(f,a,b)} approximates the integral of \code{f(x)} from \code{a} to \code{b}
	to \emph{machine} precision.

	\code{adapt...(f,a,b,tol)} integrates to a \emph{relative} error of tol.

  \code{adapt...(f,a,b,tol,trace)} displays the left end point of the current interval, the interval length, and the partial integral.

  \code{adapt...(f,a,b,tol,trace,P1,P2,...)} allows coefficients P1,
	... to be passed directly to the function f: g <-
	f(x,P1,P2,...)
}
\usage{
adaptsim(f,a,b,tol=.Machine$double.eps,trace=FALSE,...)
adaptlob(f,a,b,tol=.Machine$double.eps,trace=FALSE,...)
}
\arguments{
  \item{f}{function to be integrated.}
  \item{a}{starting abscissa of integral.}
  \item{b}{ending abscissa of integral.}
  \item{tol}{error tolerance > 0.}
  \item{trace}{if TRUE then trace will be displayed.}
  \item{...}{additional coefficients for function  \code{f}  if necessary.}
}
\value{
  The approximate value of the integral. 
}
\examples{
FexGander <- function(xx) ifelse(xx < 1,xx+1,ifelse(xx <= 3, 3 - xx, 2 ))
adaptsim(FexGander,0,5,2.0e-6,TRUE) # 7.499949 instead of 7.5
adaptlob(FexGander,0,5,2.0e-6,TRUE) # 7.500002 instead of 7.5
adaptsim(FexGander,0,5)-7.5 # -7.993606e-15 instead of 0
}
\source{
 	Walter Gautschi, 08/03/98.
  Reference: Gander, Computermathematik, Birkhaeuser, 1992.
}
\references{
Gander, W., Gautschi, W., 2000. Adaptive Quadrature - Revisited. ETH Zurich, DI IWR technical report 306. BIT 40, 1, 84--101.
}
\author{
  Christian W. Hoffmann, \email{c-w.hoffmann@sunrise.ch}, \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{math}
