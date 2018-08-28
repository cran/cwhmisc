sqrtH <- function(x) {
  stopifnot(x >= 0)
  p  <- x
  xn <- rbind(normalize(x, base = 2))
  x0 <- xn[1, ] * 2 ^ (xn[2, ] / 2)
  while(TRUE) {
    xq <- x0 ^ 2
    x1 <- x0 * (3 * p +  xq) / (p + 3 * xq)
    if (all(abs(x1 - x0) + 4 == 4))
      break
    x0 <- x1
  }# while
  res <- as.vector(t(x1)) # t -> matrix
  return (res)
} # sqrtH

pythag <- function(a, b) { # Pythagorean sum
   p <- max(abs(a), abs(b))
   q <- min(abs(a), abs(b))
   while(TRUE) {
     r <- (q / p) ^ 2
     if ( r + 4 == 4 ) break
     s <- r / ( 4 + r )
     p <- p + 2 * s * p
     q <- s * q
   }
   return(c(pyth = p, smaller = q, residual = r))
} # pythag
