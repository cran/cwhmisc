"numeric.string" <- function(str) {
  oldop <- options(warn = -1)
  on.exit(options(oldop))
  !is.na(as.numeric(str))
} 

"all.digits" <- function(str) {
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
