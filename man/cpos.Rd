\name{cpos}
\alias{cpos}
\alias{substring.location}
\title{Find the position of a substring}
\description{
  \code{cpos} finds the first position of a substring
  \code{substring.location} returns a list with starting and ending
  positions, works only with a single string.
}
\usage{
cpos(str,sub,start=1)
substring.location(str, sub, restrict)
}
\arguments{
  \item{str}{string (1-dim)}
  \item{sub}{string (1-dim)}
  \item{start}{integer}
  \item{restrict}{vector of lower and upper index the search should be
		restricted to}
}
\value{
  number, if found, NA otherwise.
  list(first,last)
}
\examples{
cpos(" Baldrian","a",5) #  8
cpos("Baldrian","B",15) # NA
substring.location("In,theese,housees,there,are,rats","ees")
#$first  [1]  6 15
#$last   [1]  8 17
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{misc}
\keyword{documentation}
