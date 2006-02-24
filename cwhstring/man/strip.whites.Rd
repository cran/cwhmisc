% comment
\name{strip.whites}
\alias{strip.whites}
\title{Strip white spaces}
\description{
  Strip white spaces from a string.
	}
\usage{
strip.whites(string)
}
\arguments{
  \item{string}{A character string (1-dim).}
}
\value{
	The cleaned character string
}
\note{
Uses system("sed...")
}
\examples{
 strip.whites("I will always be here") # WILL NOT WORK YET !?
}
\author{agw  1996-09-03  Chuck Lo Presti  Statistics Group Energy Division Pacific Northwest National Laboratory Richland WA 99352 \email{d38716@gaviota.pnl.gov} }
\keyword{character}
