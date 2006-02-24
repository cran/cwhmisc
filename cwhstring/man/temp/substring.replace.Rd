\name{substring.replace}
\alias{substring.replace}
\title{Replace a substring}
\description{
  \code{substring<-} assigns new content to a substring of a string.
}
\usage{
substring(text, first, last=100000) <- sub}
\arguments{
  \item{text}{A string to be modified.}
  \item{first}{Starting place(s) for modification in \code{text}.}
  \item{last}{Stopping place(s) for modification in \code{text}.}
  \item{sub}{The string to be inserted into \code{text}.}
}
\value{
	The modified string
}
\examples{
x <- 'this string'
substring(x, 3, 4) <- 'IS' ;x   # [1] "thIS string"
substring(x, 7) <- '' ;x        # [1] "thIS s"
substring(x) <- 'I love you' ;x # [1] "I love you"
substring(x, "bc") <- "BCX";x   # [1] "aBCXd"
substring(x, "B*d") <- "B*D";x  # [1] "aBCXD"
}
\note {
	Uses sedit, if (is.character(first) & missing(last))
  Stops if (is.character(first) & (!missing(last)))
}
\author{? + Christian W. Hoffmann, \email{hoffmann@wsl.ch}}
\keyword{character}
