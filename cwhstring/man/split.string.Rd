\name{splitString}
\alias{splitString}
\alias{getSubs}
\alias{cv}
\alias{vc}
\title{Split a string at the location(s) of a delimiting string, at the
  single characters of a delimiter}
\description{
  \code{splitString} splits string at the location(s) of a delimiting
  string
  \code{getSubs} splits string at the location(s) of the
  constituent characters of a delimiting string
  \code{cv} Convert string to vector of characters.
  \code{vc} Convert vector of characters to string.
}
\usage{
  splitString(str,pattern,clean=FALSE)
  getSubs(str, sep = ",")
  cv(str)
  vc(vec)
}
\arguments{
  \item{str}{A string.}
  \item{pattern}{The pattern acting as the delimiting string.}
  \item{clean}{If TRUE then elements "" will be discarded.}
  \item{sep}{A vector of characters or a string.}
  \item{vec}{A vector of characters.}
}
\value{
  \item{splitString}{A character vector of the parts.
	  If clean == TRUE then elements containing "" are omitted.}
  \item{getSubs}{A character vector of the parts.}
  \item{cv}{a vector of characters.}
  \item{vc}{a string.}
}
\note{
  Uses \code{\link{substring}, \link{substring.location}}.
}
\examples{
nj.string <- " 12.345   1.2345E+02  0.123"
splitString(nj.string," ",clean=TRUE)
  # [1] "12.345"     "1.2345E+02" "0.123"
splitString(nj.string,"3",clean=TRUE)
  # [1] " 12."         "45   1.2"     "45E+02  0.12"

as.numeric(getSubs(" 12.345   1.2345E+02  0.123", sep=" ")) # [1]  12.345 123.450   0.123

rev(getSubs("I want to grab this number as is 4E+03", " "))[1] #  "4E+03"

#For a vector of character strings, with the same number of elements in each string use:

x <- c(" 12.345   1.2345E+02  0.123"," 2 4.56   5.67")
c(apply(matrix(x), 1, getSubs, " "))
# [1] "12.345"     "1.2345E+02" "0.123"  "2"    "4.56"   "5.67" 

#else use:

y <- c(" 12.345   1.2345E+02  0.123", " 5 3.56")                
unlist(apply(matrix(y), 1, getSubs, " "))
#  "12.345"     "1.2345E+02" "0.123"      "5"          "3.56"   

cv("I love you")  # [1] "I" " " "l" "o" "v" "e" " " "y" "o" "u"
vc(rev(cv("I love you"))) # "uoy evol I"
}
\author{John Wallace, \email{rw@fish.washington.edu},
  (cv, vc:) Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{character}

