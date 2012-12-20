rotL <- function(phi,k=1,m=2,n=3) {
  res <- diag(n)
  ss  <- sin(phi)
  cc  <- cos(phi)
  res[k,k] <- res[m,m] <- cc
  res[k,m] <- ss
  res[m,k] <- -ss
  res
}

rotPz <- function(P) {
  rotL(-acos(P[3]/sqrt(t(P)%*%P)),1,3) %*% rotL(atan2(P[2],P[1]),1,2)
}

rotaP <- function( P, phi ) {
  r <- rotPz( P )
  t(r) %*% rotL( phi, 1, 2 ) %*% r
}
