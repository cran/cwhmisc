\name{tex.table}
\alias{tex.table}
\alias{tex.tab0}
\title{Convert a data matrix into LaTeX code.}
\description{
  These functions convert a data matrix into  \ifelse{latex}{\out{\LaTeX}}{LaTeX}.
}
\usage{
tex.table(dm, bare = FALSE, prec = if (bare) "NA" else 2,
 rnames = if (bare) "-1" else dimnames(dm)[[1]], cnames = if (bare)
 "-1" else dimnames(dm)[[2]], caption = NULL, label = NULL,
 tpos = "b", stretch = NULL, adjust = "r", file = NULL)
tex.tab0(dm, prec = 2, rnames = NULL, cnames = NULL,
  caption = NULL, label = NULL, tpos = "b", stretch = NULL,
  adjust = "r", file = NULL)
}
\arguments{
  \item{dm}{data matrix}
  \item{bare}{TRUE: \code{ prec,rnames,cnames} will get useful
		defaults, FALSE: set these parameters yourself}
  \item{prec}{precision of rounding within the LATEX table,
  if NA, then no transformation to numeric is done}
  \item{rnames}{row names}
  \item{cnames}{column names}
  \item{caption}{caption for LATEX table, default: no caption}
  \item{label}{LATEX label for the table,  default: no lable}
  \item{tpos}{position of captions: "a" for above table, "b" for below table}
  \item{stretch}{optional vector with two entries, giving the baselinestretch for the caption (stretch[1]) and the colums of the table (stretch[2]);
  default: no adjustment of baselinestretch}
  \item{adjust}{adjusts the columns of the LATEX table, default: "r" (right),                                                                       
    also possible: "l" (left) and "c" (centre) or user defined:
     "adjust=c("l","c","r",...)" yields \{l|cr...\}}
  \item{file}{output file, default: printout in console}
}
\value{
  These functions are called for their side effect to write to a file.
  \item{tex.table}{generate complete minimal Tex-able .tex file, including 'footnotesize'}
  \item{tex.tab0}{same as 'tex.table' but without 'footnotesize'}
}
\examples{
  m <- matrix(rnorm(100),nrow=10,ncol=10,dimnames=list(LETTERS[1:10],colnames=letters[1:10]))
  tex.table(m,file="tex.table.tex")
# \begin{tabular}{r|rrrrrrrrrr}
# \hline
# & a & b & c & d & e & f & g & h & i & j\\ \hline
# A & -0.63 & 1.51 & 0.92 & 1.36 & -0.16 & 0.40 & 2.40 & 0.48 & -0.57 & -0.54\\ 
# B & 0.18 & 0.39 & 0.78 & -0.10 & -0.25 & -0.61 & -0.04 & -0.71 & -0.14 & 1.21\\ 
# ...
}
\author{ ??
Adapted by:
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{interface}
