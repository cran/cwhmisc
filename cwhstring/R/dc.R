dc <- function(x,d,ch="&") { # d=0: "x&0"
##  frac <- function(x,d) {  # fractional part
##    res <- abs(x-trunc(x))
##    if (!missing(d)) res <- round(10^d*res)
##    res
##  }
  paste(trunc(x),ch,frac(x,d),sep="")
}

dcn <- function(x,d,ch="&") { # d=0: no "&"
  s <- sapply(x,function(x) eval(parse(text = paste("sprintf('%.", d, "f',", x, ")", sep = ""))))
  replacechar(s,".",ch)
}
