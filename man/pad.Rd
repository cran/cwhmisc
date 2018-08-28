\name{padding}
\alias{pad}
\alias{justify}
\alias{insstr}
\title{Padding a string with justification, insertion}
\description{ Pad a string, insert substring.
}
\usage{
  pad(str, space,just = c("right", "left", "center", "none"), with=" " )
  justify(str, space, just = c("right", "left", "center", "none"), with=" ")
  insstr(str, ins, point=nchar(str) )
}
\arguments{
  \item{str, ins}{String to be modified, to insert.}
  \item{space}{Integer, resulting length of padded string.}
  \item{just}{Mode of padding, of justification, one of "left","right","center", partial
    matching is allowed. If missing, "right" is taken, meaning
    for pad(just="r") right-ways extended (i.e. flush left), for justify(just="r") right-justified; "none" returns \code{str} unchanged.}
  \item{with}{String to pad with, will be repeated as often as necessary.}
  \item{point}{Integer, place of insertion. Appending is done for
    default value.}
}
\note{
  pad(just="r") right-ways extended (i.e. flush left),
  
  justify(just="r") right-justified,
  
  just="none" returns \code{str} unchanged.
}
\value{
  \code{pad, justify}: The string padded with \code{with}.
  
  \code{insstr} The string contained in \code{ins} inserted after character
  number \code{point} of \code{str}.
}
\examples{
pad("My string",25,"c","XoX")
 # [1] "XoXXoXXoMy stringXXoXXoXX"
pad("My string",25) # left aligned
(str <- paste00(LETTERS)) # "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
pad(str, 37, "right", " $ ")
insstr(str," $ ",7) # "ABCDEF $ GHIJKLMNOPQRSTUVWXYZ"
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character}
