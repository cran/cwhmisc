toPol <- function( x, y=0 ) {
  z <- complex( real=x,imaginary=y )
  res <- c( Mod(z), Arg(z))
  names(res) <- c("r","phi")
  return ( res )
} ## end  toPol

toRec <- function( r, phi=0 ) {
  z <- complex(modulus=r,argument=phi)
  return ( c( Re(z), Im(z) ) )
}  ## end  toRect

toSph <- function ( x,y,z, up=TRUE )  # inverse of toXyz
{
  vphi <- toPol( x,y ) # x, y -> c( w, phi )
  R <- if ( up )
            { toPol( vphi[1], z )  # ( w, z,  -> r, theta )
  } else {    toPol( z, vphi[1] )  # ( z, w,  -> r, theta )
  }
  res <- c(R[1], R[2], vphi[2])
  names(res) <- c("r","theta","phi")

  return ( res )
} ## end  toSph

toXyz <-  function ( r, theta, phi, up=TRUE )  # inverse of toSph
{
  if (up) theta <- pi/2 - theta  ###
  vz <- toRec( r, theta ) # -> if(up) v, z else z, v
  xy <- toRec( vz[2], phi )  ###
  res <- c(xy, vz[1] )
  names(res) <- c("x","y","z")
  return ( res )
} ## end  toXyz

rotZ <- function(x, y, phi) {
  rp <- toPol( x, y )
  toRec(rp[1], rp[2] + phi)
}  ## end rotZ

