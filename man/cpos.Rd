\name{cpos}
\alias{cpos}
\alias{cposR}
\title{Find the position of a substring}
\description{
  \code{cpos} finds the first position of a substring
  \code{cposR} returns a list with starting and ending
  positions, works only with a single string.
}
\usage{
cpos(str,sub,start=1)
cposR(str, sub, restrict)
}
\arguments{
  \item{str}{string)}
  \item{sub}{string)}
  \item{start}{integer}
  \item{restrict}{vector of lower and upper index the search should be
		restricted to. If missing, whole str is taken.}
}
\value{
  \code{cpos} number, if found, NA otherwise.\cr
  \code{cposR} list(first,last) for each occurrence\cr If there is none of first=NA,last=NA.
}
\examples{
cpos(" Baldrian","a",5) #  8
cpos("Baldrian","B",15) # NA
cposR("In theese housees there are rats","ees")
#$first  [1]  6 15
#$last   [1]  8 17
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{misc}
\keyword{character}
