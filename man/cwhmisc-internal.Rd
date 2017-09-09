\name{cwhmisci}
\alias{cwhmisc-internal}
\alias{.adaptsimstp}
\alias{.adaptlobstp}
\title{Functions not to be called directliy by the user.}
\description{ Recursive internal functions to adapt..
}

\usage{
.adaptsimstp(f,term,a,b,fa,fm,fb,is,trace,...)
.adaptlobstp(f,term,a,b,fa,fb,is,trace,...)
}
\arguments{
  \item{f}{function to be integrated.}
  \item{term}{function to be integrated.}
  \item{a}{starting abscissa of integral.}
  \item{b}{ending abscissa of integral.}
  \item{fa, fm, fb}{function values at a, (a+b)/2, b.}
  \item{is}{parameter to control precision.}
  \item{trace}{should intermediate steps be traced}
  \item{\ldots}{additional parameters for function f.}
}
\value{
  List (Q, term) with Q = the approximate value of the integral and term
  = the information, whether the tolerance given was too small. 
}
\author{
  Christian W. Hoffmann \email{christian@echoffmann.ch}
}
