rotA <- function( phi, P=c(0,0,1) )  { # ratation matrix to rotate around axis \code{P - O}.
##  r <- rotC( P ) ## rotate P into z-axis
  r <- rotL( -acos(P[3]/sqrt(t(P)%*%P)) ,1,3) %*% rotL( atan2(P[2],P[1]) ,1,2)
  t(r) %*% rotL( phi, 1, 2 ) %*% r
}

rotV <- function(v, w=c(0,0,1)) { # ratation matrix to rotate \code{v}  into \code{w}
  u <- vecprod(v, w)
  if ( lV(u) <= .Machine$double.eps*16.0) {
    res <- diag(3)  # v, w almost parallel
  } else {
    phi <- angle(v,w)
    res <- rotA(phi, u)
  }
  return( res )
}

rotL <- function(phi,k=1,m=2,n=3) { # Matrix \code{m} for multiplication \code{m %*% vector}.
  res <- diag(n)
  if (k != m) {
    ss  <- sin(phi)
    cc  <- cos(phi)
    res[k,k] <- res[m,m] <- cc
    res[k,m] <- ss
    res[m,k] <- -ss
  }
  res
}

getAp <- function( M ) { # determine axis and angle from rotation matrix
  e <- eigen(M)
  return(list(A=Re(e$vectors[,1]),phi=acos(Re(sum(e$values)-1)/2)))
}
