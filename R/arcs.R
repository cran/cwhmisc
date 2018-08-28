deg <- function(radian) {
  return(radian / pi * 180)
}

rad <- function(degree) {
  return(degree * pi / 180)
}

reda <- function(U, ref) {
  return(modR( U + ref * 0.5, ref ) - ref * 0.5)
}

reda2 <- function(U, V, ref) {
  n <- if (U <= V) floor( U / ref )  else floor( V / ref )
  z    <- n * ref
  return(c(U - z, V - z))
}
