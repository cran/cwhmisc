rotangle <- function(Q) {
  n <- nrow(Q)
  theta <- NULL
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (Q[j,i] != 0) {
        thk <- atan2(-Q[j,i],Q[i,i])
        theta <- c(thk,theta)
        c <- cos(thk);  s <- sin(thk)
        R <- diag(n)
        R[i,i] <- R[j,j] <- c
        R[i,j] <- -s; R[j,i] <- s
        Q <- R %*% Q
      }
      else
        theta <- c(0,theta)
    }
  }
  if (min(diag(Q)) < 0) stop("Reflection(s) occurred!")
  theta
}
