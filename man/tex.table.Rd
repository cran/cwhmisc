\name{tex.table}
\alias{exact}
\alias{tex.tabelle}
\alias{tex.table}
\alias{tex.tab.head}
\alias{tex.tab.tail}
\title{Convert a data matrix into LaTeX code.}
\description{
  These functions convert a data matrix into \LaTeX code
}
\usage{
exact(x, prec)
tex.table(datmat, bare = FALSE, precision = if (bare) "NA" else 2,
 rnames = if (bare) "-1" else dimnames(datmat)[[1]], cnames = if (bare)
 "-1" else dimnames(datmat)[[2]], caption = NULL, label = NULL,
 tpos = "b", stretch = NULL, adjust = "r", file = NULL)
tex.tab.head(datmat, precision = 2, rnames = NULL, cnames = NULL,
  caption = NULL, label = NULL, tpos = "b", stretch = NULL,
  adjust = "r", file = NULL)
tex.tab.tail(datmat, precision = 2, rnames = NULL, cnames = NULL,
  caption = NULL, label = NULL, tpos = "b", stretch = NULL,
  adjust = "r", file = NULL)
tex.tabelle(datmat, precision = 2, rnames = NULL, cnames = NULL,
  caption = NULL, label = NULL, tpos = "b", stretch = NULL,
  adjust = "r", file = NULL)
}
\arguments{
  \item{x}{numeric item}
  \item{prec}{decimal precision (?)}
  \item{datmat}{data matrix}
  \item{bare}{TRUE: \code{ precision,rnames,cnames} will get useful
		defaults, FALSE: set these parameters yourself}
  \item{precision}{precision of rounding within the LATEX table, if NA, then no transformation to numeric is done}
  \item{rnames}{row names}
  \item{cnames}{column names}
  \item{caption}{caption for LATEX table, default: no caption}
  \item{label}{LATEX label for the table,  default: no lable}
  \item{tpos}{position of captions: above or below table, "a" for above, "b" for below}
  \item{stretch}{optional vector with two entries, giving the baselinestretch for the caption (stretch[1]) and the colums of the table (stretch[2]);
  default: no adjustment of baselinestretch}
  \item{adjust}{adjusts the columns of the LATEX table, default: "r" (right), 
    also possible: "l" (left) and "c" (centre) or user defined:
     "adjust=c("l","c","r",...)" yields \{l|cr...\}}
  \item{file}{output file, default: printout in console}
}

\value{
  These functions are called for their side effect to write to a file.
}
\seealso{\code{\link[xtable]{xtable}} in package "xtable"
}
\examples{
}
\author{
Adapted by:
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{interface}
