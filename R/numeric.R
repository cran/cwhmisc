isNumeric <- function(str) {
  oldop <- options(warn = -1)
  on.exit(options(oldop))
  !is.na(as.numeric(str))
} 

scm <- function( m, n ) {n*m/gcd(m,n)}  ## Smallest Common Multiple

EulerPhi <- function(n) {sum(unlist(lapply(1:n,function(x) gcd(x,n)==1)))}

gcd <- function( m, n ) {
  x <- EuclidExtended(m,n)
  return( x[1]*m + x[2]*n )
}

EuclidExtended <- function( m, n ) {
  ## http://de.wikipedia.org/wiki/Erweiterter_euklidischer_Algorithmus
    x <- 0;    lastx <- 1
    y <- 1;    lasty <- 0
    while (n != 0) {
        (quotient <- m %/% n)
        (m0 <- n);  (n <- m %% n); (m <- m0)
        x0 <- lastx - quotient*x; lastx <- x; x <- x0; 
        y0 <- lasty - quotient*y; lasty <- y; y <- y0; 
    }
  return (c(lastx, lasty))
}

modexp <- function(a, b, n)  {  ## a^b mod n  using repeated squaring ## from http://mvngu.wordpress.com/2008/08/01/parigp-programming-for-basic-cryptography/
    bin <- intToBase( b )
    d <- a;
    for (ii in seqm(2,nchar(bin))) {
        d <- (d*d) %% n
        if (substr(bin,ii,ii) == "1")  d <- (d*a) %% n
    }
    return(d);
}
