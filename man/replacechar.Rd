\name{replacechar}
\alias{replacechar}
\title{Replace a character in a string by another}
\description{
  \code{replacechar} replaces a character in a string by another, deprecated!
}
\usage{
replacechar(str, char = "_", newchar = ".")
# is gsub(char, newchar,str)
}
\arguments{
  \item{str}{The string to be altered.}
  \item{char}{The character to be replaced.}
  \item{newchar}{The character to replace with.}
}
\value{
The altered string.
}
\examples{
replacechar("my_queer_file,name") # "my.queer.file,name"
replacechar("my_queer_file,name","m","M") # "My.queer.file,naMe"
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch> adapted from 
  tjoelker@redwood.rt.cs.boeing.com (Rod Tjoelker 865-3197)
}
\keyword{character}
