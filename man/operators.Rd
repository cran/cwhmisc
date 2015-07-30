\name{pasteInfix}
\alias{\%+\%}
\title{Paste(infix)}
\description{
  Paste as infix
}
\usage{
  a \%+\% b
}
\arguments{
  \item{a, b}{objects, will be coerced to character vectors.}
}
\value{
  see Details.
}
\examples{
"I am" \%+\% "very" \%+\%" hungry" # [1] "I am hungry"
}
\details{
  \code{\%+\%} is currently defined as \cr
  \code{"\%+\%" <- function(a,b)  paste0(a,b) }
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

