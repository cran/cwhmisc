cv <- function(str) {unlist(strsplit(str,""))} # make char vector

vc <- function(vec) {paste(vec,collapse="")}   # make string

"splitString" <- function(str,pattern,clean=FALSE){
  pattern.loc <- substring.location(str,pattern)
  if (pattern.loc$first[1] > 0 ) {
    splits <- substring(str,
			c(1, pattern.loc$last + 1), 
			c(pattern.loc$first-1, nchar(str)) 
			)
  }
  else 
    splits <- str
  if(clean == TRUE){
    splits <- splits[splits != ""]
  }
  return(splits)
}

"getSubs" <- function(str, sep = ",")
{
#   DATE WRITTEN:  26 May 1995, Last revised 15 June 1997
#   Author:  John R. Wallace (jw@u.washington.edu)
#   changed: C.Hoffmann 2006-02-03
	if(length(sep) == 1) sep <- cv(sep)
	nc <- nchar(str)
	y <- (1:nc)[cv(str) %in% sep]
	if(is.na(y[1] + 0))
		return(str)
	out <- substring(str, c(1, y + 1), c(y - 1, nc))
	if(sep == " ")
		out[out != ""]
	else out
}
