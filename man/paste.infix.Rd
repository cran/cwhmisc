\name{pasteInfix}
\alias{pasteInfix}
\alias{\%&\%}
\title{Paste(infix)}
\description{
  Paste as infix
}
\usage{
a \%&\% b
}
\arguments{
  \item{a}{an \R object, to be coerced to character vectors.}
  \item{b}{an \R object, to be coerced to character vectors.}
}
\value{
The concatenation of \code{a} and \code{b}, same as \code{ paste(a, b, sep="") }
}
\seealso{
  String manipulation with
  \code{\link{paste}},
  \code{\link{as.character}}, \code{\link{substr}}, \code{\link{nchar}},
  \code{\link{strsplit}}; further, \code{\link{cat}} which concatenates and
  writes to a file, and \code{\link{sprintf}} for C like string
  construction.
}
\examples{
"I am" \%&\% " hungry" # [1] "I am hungry"
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{misc}
\keyword{documentation}
