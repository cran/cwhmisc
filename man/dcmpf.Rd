\name{dc}
\alias{dc}
\alias{dcn}
\alias{mpf}
\title{Convert number for table columns, for equations}
\description{Convert number  \cr
- for use in decimal dot centered table columns: Replace "." in a number by "&" for LaTeX tables using
  column specification r\@\{.\}l.  \cr
-  \code{mpf(r,n)}  returns "+ r" or "- r", depending on the sign of \code{r}, with \code{n} decimal digits. Useful in \code{\link{Sweave}} files \*.Rnw for composing text for linear combinations with coefficients shown in \\Sexp{}. \cr
}
\usage{
  dc (x,d,ch="&")
  dcn(x,d,ch="&")
  mpf(r,after)
}
\arguments{
  \item{x}{Numerical vector.}
  \item{d}{Number of decimals after ".". d >= 1, will be forced internally.}
  \item{ch}{Substitute "." by \code{ch}}
  \item{after}{See \code{\link[cwhstring]{formatFix}}, the number of decimals after ".".}
  \item{r}{real value.}
}
\value{
  string representation of \code{x} suitable for table column centered
  on "."
}
\note{
\code{dc} = \code{dcn}, except for \code{ x = integer }.

\code{dc} uses \code{\link[cwhmath]{frac}}, \code{dcn} uses \code{\link{sprintf}}.
}
\examples{
  nn <- c(0, 1, 0.1, pi,2*pi,-30*pi)
  dc(nn,3)    # "0&0"     "1&0"     "0&100"   "3&142"   "6&283"   "-94&248"
  dcn(nn,3)   # "0&000"   "1&000"   "0&100"   "3&142"   "6&283"   "-94&248"

#### In file  T.Rnw:
##  <<echo=TRUE>>=
a <- -2; b <- -4; c <- 7
##  @ 
##  
##  The coefficients are: $a = \Sexpr{a}$, $b = \Sexpr{b}$, $c = \Sexpr{c}$.
##  

##  For the linear combination $$z = a + bx +cy$$ we have
##   $$z = \Sexpr{sprintf(\%.4f,a)} \Sexpr{mpf(b,3)} x \Sexpr{mpf(c,5)} y$$
#### end T.Rnw
###  Sweave: T.Rnw .. T.tex .. T.dvi


  mpf(pi,5); mpf(-pi,5)  # "+ 3.14159" "- 3.14159"  Note the space after the sign.
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{arith}
