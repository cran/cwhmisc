\name{ggrep}
\alias{grepnot}
\alias{ggrep}
\alias{countChar}
\title{Convenience functions for grep}
\description{ Grepping in (my) R directory
}
\usage{
grepnot(str,x,value=TRUE)
ggrep(opt="inr",str,dir= "/Users/hoffmannc/R/",pkg="",split=FALSE, lines=10,out=FALSE)
countChar( str, dir="/Users/hoffmannc/R/", pkg="",split=FALSE,out=FALSE)
}
\arguments{
  \item{str}{string to do grep for}\cr
  \item{x}{array of strings to check with \code{grep}.}\cr
  \item{value}{third argument to \code{\link{grep}} ('ignore.case').}\cr
  \item{opt}{options for '\code{\link{grep}}' without leading '-'}\cr
  \item{dir}{name of root directory to do grep in}\cr
  \item{pkg}{package name to do grep in; may be "" if \code{dir} itself is meant.}
  \item{split}{should \code{str} be split in single characters? If so,
    only unique characters will be searched.}
  \item{lines}{a maximum of 'lines' lines will be returned}
  \item{out}{logical, should intermediate results be printed}
}
\value{
  \item{ggrep}{grep output, with line numbers and pertaining line, or "No file with given string found".}
  \item{grepnot}{grep output, combination of invert=FALSE and =TRUE.}
  \item{countChar}{count the individual (if split=TRUE) characters in \code{str}}
}
\note{
  \code{ggrep, countChar}: both use system( grep ...)\cr
  The composed file string and the input string to grep are shown for checking.\cr
  \code{length(ggrep()) } shows number of found entries only.\cr
  If file denotes a directory,and no "r" is given, then 
  \code{opt} : contains e.g.\cr
  - "c": count lines in same one file only,\cr
  - "i": ignores case,\cr
  - "n": give file and line number,\cr
  - "r": recurse below directory one level (only),\cr
  - "v": inverts matches,\cr 
  - "w": complete word matches only,\cr
  - "x": matches must be whole lines only\cr
}
\examples{
\dontrun{
  length(ggrep("cnr","pad ",,"test*/*") )   # (dir), 10 files, not shown
  ggrep("cnr","pad ",,"test*/*")     # is dir, 10 files visited
  ggrep("cr","n2str",,"test/")
# /Users/hoffmannc/R/test/  may be a directory
# grep: /Users/hoffmannc/R/test/: No such file or directory
# NA
}
}

\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{utilities}
