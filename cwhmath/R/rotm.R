rotm <- function(n,x,y,phi) {
  res <- diag(n)
  ss  <- sin(phi)
  cc  <- cos(phi)
  res[x,x] <- res[y,y] <- cc
  res[x,y] <- ss
  res[y,x] <- -ss
  res
}
