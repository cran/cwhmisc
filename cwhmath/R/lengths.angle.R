lengths.angle <- function(x,y=NULL) {
  if (!is.null(y)) {
    x <- cbind(x,y)
  }
  w <- t(x) %*% x
  a <- sqrt(w[1,1])
  b <- sqrt(w[2,2])
  c <- if (abs(a*b) <= .Machine$double.eps) 0 else acos(w[1,2]/(a*b))
  list(lx=a, ly=b, angle=c, angleDeg=c*180/pi)
}
