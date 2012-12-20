modulo <- function( m, n  ) m %% n

moduloSymm <- function( m, n ) return  (m - (trunc( m/n )*n))

zero <- function( x ) 0.0

one  <- function( x ) 1.0

onebyx <- function( x ) 1.0/x
  
sqr <- function( x ) x^2

powr <- function( a, x ) a^x

equal <- function( x, y )  x == y

equalFuzzy <- function( x, y, prec, rel=TRUE ) abs(x - y ) <= prec*(ifelse(rel,abs(x)+abs(y),1.0))

quotmean <- function(x,y) mean(x,na.rm = TRUE)/mean(y,na.rm = TRUE)

safeDiv <- function( num, den ) {
  q <- num/den
  return( ifelse (is.infinite(q), cMAXREALBY3Q, q) )
}

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
  return (res)
}

inrange <- function(x,r) (!is.na(x) && min(r) <= x & x <= max(r))

IsCounterCl2 <- function( U, V, ref ) {
  return  ((U == V) | ((U < V) == (abs( U - V ) <= ref/2)))
} ## end  IsCounterCl2

IsCounterCl3 <- function( U, V, W, ref ) {
  if ((U == V) | (V == W)) IsCounterCl2( U, W, ref ) else {
    if (U == W) IsCounterCl2( U, V, ref ) else {
      if (U < V) ((V < W) | (W < U)) else  ## U<V<W OR V<W<U OR W<U<V *)
        ((V < W) & (W < U))
    }
  }	## end  ## If *)
} ## end  IsCounterCl3

CounterClock <- "Cntclck"; NoneClock <- "noneclck"; Clockwise <- "clckws"
ClockSense2 <- function( U, V, ref ) {
  if (U == V) NoneClock else {
    if ((U < V) == (abs( U - V ) <= ref*0.5)) CounterClock else
      Clockwise
  }
} ## end  ClockSense2

ClockSense3 <- function( U, V, W, ref ) {
  if((U == V) | (V == W)) ClockSense2( U, W, ref ) else {
    if (U == W) NoneClock else {
      if (U < V) {           ## U<V<W OR V<W<U OR W<U<V *)
        if ((V < W) | (W < U)) CounterClock else
          Clockwise
      } else {  ##if*)
      if ((V < W) & (W < U)) CounterClock else
          Clockwise
      }
    }
  }
} ## end  ClockSense3

