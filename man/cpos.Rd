\name{cpos}
\alias{cpos}
\alias{cposV}
\alias{cposR}
\alias{issubstr}
\title{Find the position of a substring}
\description{
  \code{cpos, cposV} finds the first position of a substring; \cr
  \code{cposR} returns a list with starting and ending positions, works only with a single string; \cr
  \code{issubstr} checks if is  a substring
}
\usage{
cpos(str,sub,start=1)
cposV(vstr,sub,vstart=1)
cposR(str, sub, restrict)
issubstr(str,sub,start=1)
}
\arguments{
  \item{str}{string to examine}
  \item{vstr}{vector of strings to examine}
  \item{sub}{(vector of) substring to find}
  \item{start,vstart}{(vector of) integer, position(s) of start of search}
  \item{restrict}{vector of lower and upper index the search should be restricted to. If missing, whole 'str' is taken.}
}
\value{
  \code{cpos,cposL,cposV} number, if found, NA otherwise.\cr
  \code{cposR} list(first,last) for each occurrence of \code{sub} within the restriction \code{restrict} of \code{str}; If there is none, then \code{first}=NA,\code{last}=NA.
}
\note{
  parameters in \code{cposV} will be recycled, so that all have the same (maximum) length.
}
\examples{
  cpos(" Baldrian","a",5) #  3
  cpos("Baldrian","B",15) # NA
  cposR(" Baldabcrian  abcf","abc")
#$first  6 15
#$last   8 17
  cposR(" Baldabcrian  abcf","abc",c(2:16))
#$first 6
#$last  8
  cposV(c("Xcdbeesh","withh "),c("X","h","ees"),c(1,5))
# 1 4 5
issubstr("Today is a wonderful day","wonder")
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{misc}
\keyword{character}
