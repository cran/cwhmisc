scm <- function( m, n ) {
  return(n * m / gcd(m, n))
}  ## Smallest Common Multiple

EulerPhi <- function(n) {
    sum(unlist(lapply(1 : n, function(x) gcd(x, n) == 1)))
}

gcd <- function( a, b ) {
  Euclid(a,b)[3]
}  ## end EulerPhi

Euclid <- function( a, b ) {
  ## http://en.wikipedia.org/wiki/Extended_Euclid%27s_algorithm
  S <- 0;    oldS <- 1
  T <- 1;    oldT <- 0
  R <- min(abs(c(a,b)));  oldR <- max(abs(c(a,b)))
  while (R != 0) {
      q <- oldR %/% R
      prov <- R;  R <- oldR - q * R; oldR <- prov
      prov <- S;  S <- oldS - q * S; oldS <- prov 
      prov <- T;  T <- oldT - q * T; oldT <- prov 
  }
  return (c(oldS, oldT, oldR))
}  ## end Euclid

Inv <- function(a, n) {
  T <- 0;    newT <- 1
  R <- abs(n);    newR <- abs(a)
  while (newR != 0) {
    q <- R %/% newR
    prov <- newT;  newT <- T - q * newT; T <- prov 
    prov <- newR;  newR <- R - q * newR; R <- prov
  }
  if ( R > 1)  return(NA) else {
    if(T <  0) T <- T + n
    return (T)
  }
}  # end Inv

modexp <- function(a, b, n)  { # a^b mod n
    bin <- int2B( b, 2 )[[1]]
    d <- a %% n
    for (ii in seqm(2, nchar(bin ))) {
        d <- (d * d) %% n
        if (substr(bin, ii, ii) == "1")  d <- (d * a) %% n
    }
    return(d %% n)
}  ## end modexp
