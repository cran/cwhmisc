weighted.mean1 <- function(x, w = NULL, trim = 0, na.rm = FALSE) {
  if(missing(w))
    if (length(x) & !all(is.na(x))) mean(x, trim = trim, na.rm = na.rm)
    else NA
  else  {
  	wnas <- unique(c(which(is.na(x)),which(is.na(w))))
  	if(na.rm) {
  		if(length(wnas)) {
  			x <- x[ - wnas]
        w <- w[ - wnas]
      }
  	}
  	if(trim > 0) {
  		if(trim >= 0.5)
		  	return(weighted.median(x, w, na.rm = F))
  		if(!na.rm && length(wnas))
  			return(NA)
  		n <- length(x)
	  	i1 <- floor(trim * n) + 1
  		i2 <- n - i1 + 1
      ind <- sort.list(x, unique(c(i1, i2)))
##    	x <- sort(x, unique(c(i1, i2)))[i1:i2]
      x <- (x[ind])[i1:i2]
      w <- (w[ind])[i1:i2]
  	}
    if (length(x)) sum(x*w)/sum(w)  
    else NA
  }
}
