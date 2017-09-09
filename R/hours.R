Hd <- function( h, m, s ) {
  return ( (s/60.0 + m )/60.0 + h )
} ## end  Hd

.hms <-  function( hd ) {
  h <- int( hd )
  s <- hd - h;  s <- s*60.0
  m <- int( s )
  s <- 60.0 * (s - m)
  return (list(h=h,m=m,s=s))
} # .hms

Hms <- function( hd ) {
  H <- .hms( hd )
  return ( c(H$h,H$m,H$s) )      
} ## end  Hms

Hdms <- function( hd ) {
  H <- .hms( hd )
  return ( sum(c( H$h, H$m, H$s )/c(1,10^2,10^4)) )
} ## end  Hdms

Hmsd <- function( hms ) {
  h <- int( hms )
  s <- hms - h;  s <- s*100.0
  m <- int( s )
  s <- 100.0 * (s - m)
  return ((s/60.0 + m )/60.0 + h)
} ## end  Hmsd
