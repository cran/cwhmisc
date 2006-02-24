% comment
\name{sedit}
\alias{sedit}
\alias{replace.substring.wild}
\alias{translate}
\title{Text replacement with wild characters}
\description{
  \code{sedit} A replacement for "sed", to be used under Windows
  \code{replace.substring.wild} Replace 
  \code{translate} A wrapper function for Unix "sed"
}
\usage{
sedit(text, from, to, test=NULL, wild.literal=FALSE)
replace.substring.wild(text, old, new, test=NULL, front=FALSE,back=FALSE)
translate <- function(text, old, new, multichar=!(under.unix <- TRUE))
      }
\arguments{
  \item{text}{A character string.}
  \item{from}{A character string to be substituted.}
  \item{to}{A character string to be substituted with.}
  \item{test}{Whether to ???.}
  \item{wild.literal}{Whether to ???.}
  \item{old}{A character string to be substituted.}
  \item{new}{A character string to be substituted with.}
  \item{front}{Whether to start at beginning of \code{text} ???.}
  \item{back}{Whether to end at end of \code{text} ???.}
  \item{multichar}{Should be FALSE under Unix.}
}
\value{
	A string
}
\examples{
#  THEY ARE NOT YET FUNCTIONING ???
#translate("adndfu123979879","[a-f]","[A-F]")     #  "ADnDFu123979879"  as expected
#system('tr "[a-f]" "[A-F]"', "adndfu123979879" )  #  "ADnDFu123979879"
#system('sed -e "s/[a-f]/[A-F]/g"', "adndfu123979879" )   #  "[A-F][A-F]n[A-F][A-F]u123979879"
#translate("adndfu123979879","[a-f]","[A-F]",TRUE)   #  "[A-F][A-F]n[A-F][A-F]u123979879"
#translate("adndfu123979879","[a-f]","WZ",TRUE)   #   "WZWZnWZWZu123979879"
#translate("adndfu123979879","dn","DN")  #  "aDNdfu123979879"
#translate("adndfu123979879","7","X")  #  "adndfu1239X98X9"
}
\author{unknown}
\keyword{character}
