contfrac <- function(x, depth = 13, f=floor) {
  fac <- 10 ^ (-3)
  sgn <- sign(x)
  xi  <- x <- abs(x)
  ai <- a  <- f(xi)
  ii <- 1
  while(( (1.0 + abs(ai - xi) * fac) != 1.0) && (ii <= depth)) {
    xi <- 1.0 / (xi - ai)
    ai <- f(xi)
    a  <- c(a, ai)
    ii <- ii + 1
  }
  return(a * sgn)
}

evalcfr <- function(cf) {
  sgn <- sign(cf[1])
  cf  <- abs(cf)
  res <- cf[length(cf)]
  for (ii in rev(seqm(1,length(cf) - 1))) res <-cf[ii] + 1.0 / res
  return(res * sgn)
}

toCFrac <- function(x, depth = 5) {
  REL <- 10 ^ (-7)
  c38 <- .Machine$double.xmax ^ (3 / 8)
  sig  <- sign(x)
  x    <- abs(x)
  minr <- 1.0 / .Machine$integer.max
  n    <- 1
  numnew <- 1.0
  dennew <- 0.0
  num  <- int(x)
  den  <- 1.0
  valnew <- num / den
  valold <- 0.0
  difold <- .Machine$integer.max
  difnew <- abs(valnew - valold)
  r    <- x - num
  while((n < depth) & abs(difnew - difold) > REL * abs(difold) & (r >= minr)) {
   n <- n + 1
   onebyr <- 1 / r + 8. * minr  # force some rounding
   b <- int(onebyr)
   r <- onebyr - b
   numold <- numnew
   denold <- dennew
   numnew <- num
   dennew <- den
   num    <- numnew * b + numold
   den    <- dennew * b + denold
   valold <- valnew
   valnew <- num / den
   difold <- difnew
   difnew <- abs(valnew - valold)
  }
  return(list(num = sig * num,den = den,error = abs(x - num / den)))
} # end toCFrac

toCFrac2 <- function(x, depth = 5) {
    .Machine$double.xmax ^ 0.75
  c38 <- .Machine$double.xmax ^ (3 / 8)
  sig  <- sign(x)
  x    <- abs(x)
  minr <- 1.0 / .Machine$integer.max
  n    <- 2
  num  <- c(1,int(x),rep(NA,depth - 1))
  den  <- c(0,1,rep(NA,depth - 1))
  valnew <- num[2] / den[2]
  valold <- 0.0
  difold <- .Machine$integer.max
  difnew <- abs(valnew - valold)
  r    <- x - num[2]
  while((n <= depth) & (r > c38) < (r >= minr)) {
   n <- n + 1
   onebyr <- 1 / r + 8.0 * minr  # force some rounding
   b <- int(onebyr)
   r <- onebyr - b
   num[n]  <- num[n - 1] * b + num[n - 2]
   den[n]  <- den[n - 1] * b + den[n - 2]
   valold <- valnew
   valnew <- num[n] / den[n]
   difold <- difnew
   difnew <- abs(valnew - valold)
  }
  return(list(num = sig * num[-1],den = den[-1]))
}
