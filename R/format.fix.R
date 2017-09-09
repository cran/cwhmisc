"formatFix" <- function(x,after=2,before=1,extend=TRUE) {  # 2001-08-29, C.Hoffmann, bug for x > 10^8 detected 2015-10-21
  xx <- as.vector(x)
  stripform <- function(x,afterS,len) { # 
    st <- format(x,digits=min(max(1,afterS),22),trim=TRUE,nsmall=afterS)
    difflen <- nchar(st) - len
    while (difflen < 0) {
      st <- paste(" ",st,sep="")
      difflen <- difflen+1
    }
    while ((difflen > 0) & (substring(st,1,1) == " ")) {
      st <- substring(st,2)
      difflen <- difflen-1
    }
    if (difflen) paste(rep("*",len),collapse="")
    else st
  }  ### stripform
  
  maxA  <- 1.0e8
  aft <- max(after,0)
  withdot <- aft > 0
  toobig  <- ifelse(is.na(xx),TRUE,abs(xx)>=maxA)
  decim   <- pmax(floor(log10(abs(xx))*(1+.Machine$double.eps)),0)
  reqbef  <- ifelse(is.na(xx),2,pmax(decim,0) + as.numeric(xx<0) + 1)
  placesbefore <- ifelse(is.na(xx),2,ifelse(rep(extend,length(xx)),decim+2,pmin(before,reqbef)))
  placesbefore[toobig] <- 0  ## ???
  xx     <- round(abs(xx)*10^aft)  #  treat as integer
  before <- max(before,placesbefore)
  filldot <- reqbef > before
  regular <- !filldot & !toobig
  len <- mlen <- before+aft+1
  if (!withdot) mlen <- mlen-1
  if (extend & any(toobig)) {
    ncc <- max(nchar(format(xx[toobig],digits=min(max(1,aft),22)))) - mlen
    if (ncc>0) {mlen <- mlen+ncc; len <- len+ncc}
  }
  str <- matrix("*",mlen,length(x))
  str[,regular] <- " "
## catn("regular = ", regular)
## cat("str = ");print(str)
  if (any(regular)) {
    kk <- 1
    while (kk <= aft) {
      str[len-kk+1,regular] <- xx[regular] %% 10
      xx[regular] <- xx[regular] %/% 10
      kk <- kk+1
    }
    if (withdot) str[len-kk+1,regular] <- "."
    while (max(xx[regular]) > 0 | kk == aft+1) { # latter for leading 0
      str[len-kk,regular] <- ifelse(xx[regular] > 0 | kk == aft+1,xx[regular] %% 10,str[len-kk,regular])
      xx[regular] <- xx[regular] %/% 10
      kk <- kk+1
    }
    str[cbind(len-aft-placesbefore,seq(ncol(str))) [regular & (x<0),]] <- "-"
  }
  res <- apply(str,2,paste,collapse="")
##  catn("toobig = ", toobig,", res = ", res)
  if (any(toobig)) res[toobig] <- sapply(xx[toobig],stripform,aft,mlen)
  names(res) <- names(x)
  if (is.array(x)) dim(res) <- dim(x)
  return ( res )
} # formatFix

