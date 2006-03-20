\name{seqm}
\alias{seqm}
\title{sequences, empty if "by" not conforming}
\description{
  Generate sequences, but return NULL, when "seq" would generate an error.This function is useful for \code{\link{for}}-loops, when empty
  loops are required, if \code{by} is in the "wrong" direction, see \emph{examples}.
}
\usage{
  seqm(from, to, by=1)
}
\arguments{
  \item{from}{starting value of sequence.}
  \item{to}{(maximal) end value of the sequence.}
  \item{by}{increment of the sequence.}
}
\value{
  \code{NULL}, if (to-from)*by <0, otherwise
  usual result of \code{\link{seq}} i.e. seq.default.
}
\examples{
  seqm(12,4,-1)  #  12 11 10  9  8  7  6  5  4
  seqm(12,4,2)   #  NULL
  lo <- 1; up <- 3
  for (ii in lo:up) {
    cat(ii,"    ")
    for (kk in seqm(lo,ii-1)) {
      cat("   ",kk)  # do-in-lower-triangle
    }
    cat(" diag")     # do-something-on-the-diagonal
    for (kk in seqm(ii+1,up)) {
      cat("  :",kk)  # do-in-upper-traingle
    }
    cat("\n")    
  }
# 1      diag  : 2  : 3
# 2         1 diag  : 3
# 3         1    2 diag
}
\keyword{arith}