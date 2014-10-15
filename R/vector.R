angle <- function(v, w)  {
  cs <- v %*% w
  lp <- lV(v)*lV(w)
  pc <- cs/lp
  if (abs(pc - 1) >= .Machine$double.eps*128.0 ) {
    return(acos( pc ) )
  } else {  # small angles use asin instead of acos
    vp <- lV(vecprod(v, w))
    return(asin(vp/lp))
  }
  acos( ( v %*% w )/(lV(v)*lV(w)) )
}

lV <- function( v ) sqrt(v %*% v)

vecprod <- function(v, w) { # v cross w
  return( c(v[2]*w[3]  - v[3]*w[2],
            v[3]*w[1]  - v[1]*w[3],
            v[1]*w[2]  - v[2]*w[1] ))
} # end vecprod

"%v%" <- function(v,w) vecprod(v,w)
