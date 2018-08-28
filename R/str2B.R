str2B <- function(str, base=10, round = 0) {
# round to 'round'
  x <- gsub(" ", "", str) # eliminate blanks
  v <- 1
  switch ( substring(x, 1, 1),
     "-" = {v <- -1; x <- substring(x, 2, 100)},
     "+" = {x <- substring(x, 2, 100)},
     0 # no sign, do nothing
   )
  dot <- cpos(x, ".")
  x   <- sub("\\.",  "", x)
  ch  <- unlist(strsplit(x, ""))
  len <- length(ch)
  if (is.na(dot)) {
    dot    <- len + 1
    round <- min(0, round)
  }
  ch  <- as.numeric(ch)
  start <- min(dot + round - 1, len)
  if (start < 0) {
    res <- 0
  } else {
    round <- start - dot + 1
    sumb <- 0
    roun <- ch[start + 1] / base
    if (start > 0) {
      for (kk in seqm(1, start)) {
        sumb <- sumb * base + ch[kk]
      } # for
    }
    if (kk <= len - 1) sumb <-  sumb + trunc(roun + 0.5)
    res <- sumb * v * 10 ^ (-round)
  } # else res <- 0
  return(res)
} # str2B
