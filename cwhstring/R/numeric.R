"numeric_string" <- function(str) {
  oldop <- options(warn = -1)
  on.exit(options(oldop))
  !is.na(as.numeric(str))
} 

"all_digits" <- function(str) {
  k <- length(str)
  result <- logical(k)
  for(i in 1:k) {
    st <- str[i]
    ls <- nchar(st)
    ex <- substring(st, 1:ls, 1:ls)
    result[i] <- all(match(ex,c('0','1','2','3','4','5','6','7','8','9'),nomatch=0)>0)
  }
  result
}

intToASCII <- function(i) {
  ASCII[i %% 256];
}

intToBase <- function(i,Base=2) {
  stopifnot(2 <= Base & Base <= 16)
  res <- HexDig[i %% Base + 1]
  i <- i - i %% Base
  while (i > 0) {
    i <- i %/% Base
    res <- paste(HexDig[i %% Base + 1],res,sep="")
    i <- i - i %% Base
  }
  res
}

intToOct <- function(i) {
  intToBase(i,8)
}

intToHex <- function(i) {
  intToBase(i,16)
}
