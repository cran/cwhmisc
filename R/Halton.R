HS247 <- function(K, N, R, P=rep(0, K)) {
  Q <- matrix(nrow = N, ncol = K)
  for (ii in 1:K) {
    r <- 1.0 / R[ii]
    for (nn in 1:N) {
      f <- if (nn > 1) 1.0 - Q[nn - 1, ii] else 1.0 - P[ii]
      g <- 1.0
      h <- r
      while (f - h < 0.1) {
        g <- h
        h <- h * r
      }
      Q[nn, ii] <- g + h - f
    }
  }
  return(Q)
}
