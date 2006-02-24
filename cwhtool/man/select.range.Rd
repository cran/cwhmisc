\name{select.range}
\alias{select.range}
\title{Select values from a vector depending on a range in a second vector.}
\arguments{
  \item{groupvec}{a vector of observations to be used for grouping.}
  \item{min}{the minimum value of the range.}
  \item{max}{the maximum value of the range.}
  \item{data}{a numeric vector of observations.}
}
\description{
  \code{select.range} accepts two vectors of paired observations and
  returns a vector of observations from \code{data}.  The observations
  returned are those for which the paired values in \code{groupvec} are
  within the range specified by \code{min} and \code{max}.
  NOTE: The in-range condition is \emph{greater than or equal to} \code{min}
  and \emph{less than} \code{max}.  This allows contiguous ranges to be
  specified without returning the same value in two sets.
}
\usage{
select.range(groupvec, min, max, data)
}
\value{
  The subset of observations from \code{data} is returned invisibly.
}
\examples{
testvec <-c(2,4,3,5,4,5,7,6,4,5,6,8,7,9,8)
agevec  <-c(10,13,14,25,29,32,34,45,48,55,62,67,69,70,74)
newvec  <-select.range(agevec,0,30,testvec)
newvec  # [1] 2 4 3 5 4
}
\author{?}
\keyword{array}
