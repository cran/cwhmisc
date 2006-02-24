weighted.median <- function(x, w = NULL, na.rm = FALSE)
{
  if (missing(w))
    median(x, na.rm)
  else {
  	if(mode(x) != "numeric" | mode(w) != "numeric")
  		stop("need numeric data")
  	x <- as.vector(x)
  	w <- as.vector(w)
  	wnas <- unique(c(which(is.na(x)),which(is.na(w))))
	  if(length(wnas)) {
	  	if(na.rm) {
	  		x <- x[ - wnas]
        w <- w[ - wnas]
      }
		  else return(NA)
  	}
    ind <- sort.list(x)
  	cw <-sum(w)/2+0.5 ## *length(w) # mimics 1:n, but weighted
    half <- seq(along=w)[diff(cumsum(w[ind]) == cw)==-1]  ## find place where ==
  	if (length(half)) {  # take the exact value
  		(x[ind])[half]
  	}
  	else {  ## take half of straddling values
  		half <- seq(along=w)[abs(diff(cumsum(w[ind]) > cw))==1] + 0:1
  		sum(x[ind][half])/2
  	}
  }
}

