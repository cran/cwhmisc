\name{napply}
\alias{napply}
\title{Apply a function to the corresponding elements of two lists (?)}
\description{
  lapply extended for two lists ??
}
\usage{
 napply(...,FUN)
}
\arguments{
  \item{...}{list of vectors ??}
  \item{FUN}{function to be used}
}
\value{
 list of values
}
\examples{
%I'm now using a for loop and applying union() to each pair of vectors, but is there a faster way avoiding the for ?
l1 <- 2
l2 <- 4
napply(l1,l2,FUN=union)
}
\author{Peter Dalgaard, \email{p.dalgaard@biostat.ku.dk}
}

\keyword{manip} %-- one or more ...
