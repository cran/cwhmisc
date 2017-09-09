\name{remove.dup.rows}
\alias{remove.dup.rows}
\title{Remove duplicate rows}
\description{
  Removes duplicate rows from a dataframe.
}
\usage{
remove.dup.rows(dfr)
}
\arguments{
  \item{dfr}{A dataframe}
}
\details{
  Uses the function eql.
}
\value{
  The dataframe with only one copy of identical rows.
}
\examples{
  dfr <- data.frame(matrix(c(1:3,2:4,1:3,1:3,2:4,3:5),6,byrow=TRUE))
  remove.dup.rows(dfr)
}
\author{
Peter Dalgaard, \email{p.dalgaard@biostat.ku.dk}
}
\keyword{manip}
\keyword{logic}
