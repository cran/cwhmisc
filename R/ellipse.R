ellipse <- function(k, m, A = NULL, cn = NULL, a = NULL, b = NULL, phi = NULL)
{
  if (missing(A)) { # convert to call with A
    warning("not yet tested")
    A <- rotm(2,1,2,phi) %*% matrix(c(a,0,0,b), 2, 2)
  }
  else {#A <- a; cn <- b  quick fix 2004-09-03
      }
  k <- max(k, 4)
  r <- A[1, 2]/sqrt(A[1, 1] * A[2, 2])
  Q <- matrix(0, 2, 2)			 # construct matrix Q for
  Q[1, 1] <-  sqrt(A[1, 1] * (1+r)/2)    # transformation of circle
  Q[1, 2] <- -sqrt(A[1, 1] * (1-r)/2)	 # to ellipse
  Q[2, 1] <-  sqrt(A[2, 2] * (1+r)/2)
  Q[1, 1] <-  sqrt(A[2, 2] * (1-r)/2)
  alpha <- seq(0, by = (2 * pi)/k, length = k)	   # define angles
  Z <- cbind(cos(alpha), sin(alpha))     # points on unit circle
  X <- t(m + cn * Q %*% t(Z))            # coordinates of points on ellipse
  X
}					 # end of procedure ellipse

conf.ellipse <- function(k, m, A, df, level = 0.95)
{
  k <- max(k, 4)
	d <- sqrt(diag(A))
	dfvec <- c(2, df)
	phase <- acos(A[1, 2]/(d[1] * d[2]))
	angles <- seq( - (pi), pi, len = k)
	mult <- sqrt(dfvec[1] * qf(level, dfvec[1], dfvec[2]))
	xpts <- m[1] + d[1] * mult * cos(angles)
	ypts <- m[2] + d[2] * mult * cos(angles + phase)
	cbind(xpts, ypts)
}
