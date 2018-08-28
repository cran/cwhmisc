cpos <- function(str, sub, start = 1) {
  lstr  <- nchar(str)
  lsub1 <- nchar(sub) - 1
  if (start + lsub1 > lstr) return(NA)
  else {
    str <- substring(str, 1:(lstr - lsub1), (1 + lsub1):lstr)
    p <- which(str == sub)[1]
    if (is.na(p > 0)) return(NA)
    else return(p)
  }
}

cposV <- function(vstr, sub, vstart = 1) {
  Len <- max(length(vstr), length(sub), length(vstart))
  vstr  <- matrix(c(rep(vstr, len = Len), rep(sub, len = Len),
             rep(vstart, len = Len)), ncol = 3)
  re <- sq <- 1:Len # generate vector with right length
  for (ii in sq) {
    re[ii] <- cpos(vstr[ii, 1], vstr[ii, 2], as.integer(vstr[ii, 3]))
  } # ii
  return(re)
} # cposV

cposR <- function(str, sub, restrict = c(1, nchar(str))) {
  if(length(str) > 1) stop('only works with a single string str')
  l.str <- nchar(str)
  l.sub <- nchar(sub)
  if(l.sub > l.str) return(list(first = 0, last = 0))
  if(l.sub == l.str)  return(if(str == sub) list(first = 1,
               last = l.str) else     list(first = 0, last = 0))
  is <- 1:(l.str - l.sub + 1)
  ss <- substring(str, is, is + l.sub - 1)
  k <- ss == sub
  if(!any(k)) return(list(first = NA, last = NA))
  First <- k <- is[k]
  Last  <- k + l.sub - 1
  irF <- inrange(First, restrict)
  irL <- inrange(Last, restrict)
  ir <- irF & irL
  if(length(k) == 0 | !any(ir)) {list(first = NA, last = NA)
  } else list(first = First[ir], last = Last[ir])
}

issubstr <- function(str, sub, start = 1) {
  oldop <- options(warn = -1)
  on.exit(options(oldop))
  return(!is.na( cpos(str, sub, start)))
} # issubstr
