chsvd <- function(s){ s$u %*% diag(s$d) %*% t(s$v) }

divmod <- function(i,n) c(i %/% n, i %% n)

divmodL <- function(i,n) list(d=i %/% n, m=i %% n)

dsm <-  function( x, w ) {
   dm <- divmod( x, sum(w) )
   sm <- submod( dm[2], cumsum(w) )
   res <- c( dm[1], sm )
   return( res )
 } # dsm

equal <- function( x, y )  x == y

equalFuzzy <- function( x, y, prec=8*.Machine$double.eps, rel=TRUE ) {
  abs(x - y ) <= prec*(ifelse(rel,abs(x)+abs(y),1.0))
} # equalFuzzy

exch <- function(x,L,R) {
  indL <- which( x == L )
  indR <- which( x == R )
  x[indL] <- R
  x[indR] <- L
  return ( x )
} # exch

frac <- function(x,d) {  # fractional part
  res <- abs(x-trunc(x))
  if (!missing(d)) res <- round(10^d*res)
  res
} # frac

int  <- function ( x ) as.integer(trunc(x))

inrange <- function(x,y) (!is.na(x) && min(y) <= x & x <= max(y))

last <- function(x) rev(x)[1] # last element

K <- function( z ) (z- 1i)/(z+1i)

Km <- function( z ) (-1i)*(z + 1)/(z - 1) # inverse of K

LE <- function(x) length(x) # last index

LS <- function() .Last.value

lV <- function( x ) as.vector( sqrt( x %*% x ) )

mod <- function( x, y  ) x %% y

modR <- function( x, y ) return  (x - (floor( x/y )*y))

modS <- function( x, y ) return  (x - (trunc( x/y )*y))

norm2 <- function( x ) {
  s <- 0
  for ( kk in seq_along( x ) ) {
    s <- pythag( s, x[kk] )[1]
  } # kk
  return( s )
} # norm2

one  <- function( x ) 1.0

onebyx <- function( x ) 1.0/x
  
powr <- function( a, x ) a^x

pythag <- function( a, b ) { # Pythagorean sum
   p <- max ( abs ( a), abs( b ) )
   q <- min ( abs ( a), abs( b ) )
   while ( TRUE ) {
     r <- (q/p)^2
     if ( r + 4 == 4 ) break
     s <- r / ( 4 + r )
     p <- p + 2*s*p
     q <- s*q
   }
   return( c(p,r))
 } # pythag

quotmean <- function(x,y) mean(x,na.rm = TRUE)/mean(y,na.rm = TRUE)

safeDiv <- function( num, den ) {
  q <- ifelse (num==0 & den==0, 1,  num/den)
  return( ifelse (is.infinite(q), c3Q, q) )
} # safeDiv

signp <- function(x) ifelse( is.complex(x), NA, ifelse( is.na(x) | !is.finite(x) | x>=0 , 1, -1 ) )

sqr <- function( x ) x^2

solveQeq <- function(a,b,c) { # solve ax^2 + bx + c = 0 for x
  ab <- abs(b) + abs(c)
  if (Mod(a)*0.125 + ab == ab) { 
    # a almost 0 to within 8*.Machine$double.eps
    res <- -c/b
    if (is.infinite(res)) res <- c(NA,NA) 
  } else {
    dis <- b^2 - 4*a*c
    if ((Im(dis) == 0) && (Re(dis) < 0.0)) {  # complex case
      dis <- complex(real=dis)
    }
    x1 <- -signp(b)*(abs(b) + sqrt(dis))/a*0.5
    res <- if (dis!=0.0) c(x1,c/a/x1) else c(x1,x1)
  }
!  return (res)
} # solveQeq

sqr <- function( x ) x^2

sqrtH <- function( x ) {
  stopifnot( x >= 0 )
  xn <- rapply( normalize(x,2), function(y) y, how = "unlist")
  x0 <- xn[1]*2^(xn[2]/2); names( x0 ) <- ""
#  cnt <- 0
  p <- x
  while ( TRUE ) {
#    cnt <- cnt+1
    xq <- x0^2
    x1 <- x0*(3*p +  xq)/(p + 3*xq)
      if ( abs(x1 - x0) + 4 == 4 # || cnt>10
          ) break
    x0 <- x1
  }# while
  return ( x1 )
} # sqrtH

submod <- function(x, v) {
  if (x <= 0) return ( c(0,0) )
  v <- c(0, v)
  ii <- sum( v < x  )
  return( c( ii-1, x - v[ii]) )
}  # submod

zero <- function( x ) 0.0
