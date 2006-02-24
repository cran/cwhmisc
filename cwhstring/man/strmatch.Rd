\name{strmatch}
\alias{strmatch}
\title{A "shortest unique identifier" match.}
\description{
	If an input string matches no output string, return NA.

	If it is an ambiguous match, return 0.

	Otherwise return the index of the match.

	Exception: if the target string contains "log" and "logistic", and the
   user type "lo" it is ambiguous, but if he types "log" consider it a
   perfect match.
}
\usage{
strmatch(inputs, target)
}
\arguments{
  \item{inputs}{The string to be found as a match.}
  \item{target}{The string which should contain the match.}
}
\value{
	The matched string, if it exists.
	NA, if \code{input} matches no output string.
	0 , if ambiguous matches are found.
}
\examples{
# strmatch(c("he","we"), c("She will","in awe","xxxx"))
}
\note{
Uses  .C("strmatch",  which is Not Found in R ??! 
}
\author{? from SCCS: \@(\#)strmatch.s	2.2  1989-09-27}
\keyword{character}

