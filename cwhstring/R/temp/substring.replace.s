substring.replace <- function(text, first=1, last=nchar(text), value) {
  if(is.character(first)) {
   if(!missing(last)) stop('wrong # arguments')
   return(sedit(text, first, value))  
 }

  lf <- length(first)

  if(length(text)==1 && lf > 1) {
  	if(missing(last)) last <- nchar(text)
  	last <- rep(last, length=lf)
  	for(i in 1:lf) {
  	  text <- paste(if(first[i]>1) 
              substring(text, 1, first[i]-1), value,
        	    substring(text, last[i]+1), sep='')
	    if(i < lf) {
	      j <- (i+1):lf
	      w <- nchar(value) - (last[i]-first[i]+1)
	      first[j] <- first[j] + w  
	      last[j] <- last[j] +  w
	    }
   	}
  	return(text)
  }
  res <- paste(ifelse(first>1,substring(text, 1, first-1),''), value,
              substring(text, last+1), sep='')
  res
}
