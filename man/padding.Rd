\name{padding}
\alias{padding}
\title{Padding a string with justification}
\description{
  \code{padding} Pads a string.
}
\usage{
padding(str, space, with, to=c("left","right","center"))}
\arguments{
  \item{str}{String to be padded.}
  \item{space}{Resulting length of padded string.}
  \item{with}{String to pad with. Will be repeated as often as necessary.}
  \item{to}{Mode of padding, one of "left","right","center".}
}
\value{
A string
}
\examples{
padding("My string",25,"XoX","center")
 # [1] "XoXXoXXoMy stringXXoXXoXX"
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character}
