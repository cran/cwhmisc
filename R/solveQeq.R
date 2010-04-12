solveQeq <- function( A, B, C ) {
  # solve Ax^2 + Bx + C = 0, A ≠ 0. Avoid cancellation by
  # computing the (real) root with the greatest absolute value first
  if (A == 0) {
    return ( -C/B )
  } else {
    S <- B^2 - 4*A*C
    if (S >= 0.0)  {
      D <- -signp(B)*(abs(B) + sqrt(S))
      res <- vector ("numeric", 2 )
    } else {
      D <- -signp(B)*(abs(B) + sqrt(as.complex(S)))
      res <- vector ("complex", 2 )
    }
    res <-c(0.5*D/A, 2*C/D)
    return( res )
  }
}
