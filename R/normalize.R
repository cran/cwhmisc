Nd <- function(x, base = 10) {
  return(floor(log(ifelse(x == 0, 1, abs (x)), base = base)))
}

sigplaces <- function(x, base = 10, rnd = 0) { # no of places for trunc(x)
    s <-  ifelse(is.na(x) & !is.nan(x), 2, ifelse(is.na(x) | (!is.finite(x) &             (x > 0) | is.nan(x)), 3, ifelse(!is.na(x) & !is.finite(x) &
             (x < 0), 4, ifelse(x == 0, 1, ifelse(log(abs(x)) > 0,
             Nd(x,base) + 1, rnd + 1)))))
  return(s)
} # sigplaces

normalize <- function( x, base = 2 ) {
  ##return a and e so that  x  = a*base ^ e,   abs( a ) in [1, base)
  B1 <- (base < 1) 
  if ( B1 ) base <- 1 / base
     .normalize0 <- function( y, base = 2 ) {
       e <- trunc(log(abs(y),base = base))
       if (is.na(y) ) {
         a <- e
         e <- 0
       } else if (abs(e) == Inf | is.na(e)) {
         a <- e
         e <- 0
         if (y == 0) {
           a <- 0
           e <- 0
         }
       } else {
         a <- y / ( base ^ e )
         if ( abs(a) < 1.0 ) {
           e <- e - 1.0
           a <- a * base
         } else if (abs(a) > base ) {
             e <- e + 1.0
             a <- a / base }
       } # else a < y
       return(c(a + 0.0, e, base))
     } #.normalize0
  Ncol <- length(x)
  re <- as.data.frame(matrix(NA, nrow = 3, ncol = Ncol),
                row.names = c("a", "e", "b" ) )
  for ( ii in 1:Ncol ) {
    re[ii] <- sapply( x[ii], .normalize0, base, USE.NAMES = FALSE )
  } # ii
    if ( B1 ) {
        re[2,] <- -re[2,]
        re[3,] <- 1 / re[3,]
    }
  return(re)
} # normalize

normalize1 <- function( x, base = 2 ) {
  xx <- normalize( x, base )
  xx[1,] <- xx[1,] / base
  xx[2,] <- xx[2,] + 1
  return( xx )
} # normalize1

checkNormalize <- function ( no ) apply(no, 2, function(o) o[1] * o[3] ^ o[2] )
