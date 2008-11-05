% comment
\name{replacechar}
\alias{replacechar}
\title{Replace a character in a string by another}
\description{
  \code{replacechar} replaces a character in a string by another
}
\usage{
replacechar(str, char = "_", newchar = ".")
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
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{character}
