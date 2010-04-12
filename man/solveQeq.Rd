\name{solveQeq}
\alias{solveQeq}
\title{Solve linear and quadratic equations.}
\description{
  Compute the solution(s) avoiding cancellation in the real case.
  
}
\usage{
solveQeq(A, B, C)
}
\arguments{
  \item{A, B, C}{coefficients in Ax^2 + Bx + C = 0.}
}
\value{
  The solution(s)
}
\examples{
  solveQeq(0,-1,-1) # -1
  solveQeq(0,-1,0)  # 0
  solveQeq(0,0,0)   # NaN
  solveQeq(1,1,1)   # -0.5-0.866025i, -0.5+0.866025i
  solveQeq(1,-1,1)  #  0.5+0.866025i,  0.5-0.866025i

}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{algebra}

