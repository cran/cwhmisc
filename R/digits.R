allDigits <- function ( str, base=10 ) {
  Legal <- HexDig[ 1:(base) ]
  legal <- union( Legal, lowerize( Legal ) )
  k <- length(str)
  res <- logical(k)
  for (i in 1:k) {
    st <- str[i]
    ex <- unlist( strsplit(st, NULL) )
    res[i] <- all(match(ex, legal, nomatch = 0) > 0)
  }
  return( res )
} # allDigits

isNumeric <- function (str) {
    oldop <- options(warn = -1)
    on.exit(options(oldop))
    !is.na(as.numeric(str))
} # isNumeric

str2dig <- function( str ) {
  k <- length(str)
  res <- logical(k)
  for(i in 1:k) {
    st <- str[i]
    res[i] <- as.numeric( unlist(strsplit(st,NULL)) )
  }
  return( res )
} # str2dig
