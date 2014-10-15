\name{pasteInfix}
\alias{\%&\%}
\title{Paste(infix)}
\description{
  Paste as infix in two variants
}
\usage{
  a \%&\% b
}
\arguments{
  \item{a, b}{objects, will be coerced to character vectors.}
}
\value{
  Same as \code{ paste(a, b, sep=)}
}
\examples{
"I am" \%&\% " hungry" # [1] "I am hungry"
}
\details{
  \code{\%&\%} is currently defined as \cr
  \code{"\%&\%" <- function(a,b)  paste(a,b,sep="") }
}
\seealso{
  String manipulation with
  \code{\link{paste}},
  \code{\link{as.character}}, \code{\link{substr}}, \code{\link{nchar}},
  \code{\link{strsplit}}; further, \code{\link{cat}} which concatenates and
  writes to a file, and \code{\link{sprintf}} for C like string
  construction.
}
\author{christian@echoffmann.ch>
}
\keyword{misc}
\keyword{documentation}  

