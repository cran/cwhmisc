ellipseC <- function(mid, a, b=a, ra=c(-1,361), phi=0, k=a*100 )
{
  k <- abs( mean( k ) ) # de-vectorize
  ra <- rad(ra)
  alpha <- seq(0, by = (2 * pi)/k, length.out = k+1)
# define angles, overlapping
  c1 <- alpha >= min(ra)
  c2 <- alpha <= max(ra)
  ra <-  (c1 & c2)
  ran <- alpha[ra]
  Z <- complex( real=a*cos(ran), imaginary=b*sin(ran))
  R <- exp(phi*1i)
  res <- Z * R + mid
  return( res )
}  # end of ellipseC

ellipse1 <- function( a, b=a, ra=c(-1,361), phi=0, k=a*100 )
{ ra <- ellipseC( 0, a, b=b, ra=ra, phi=phi, k=k )
  res <- cbind( Re( ra ), Im( ra ) )
  return( res )
}  # end of ellipse

conf.ellipse <- function( a, b, phi, df1, df2, level = 0.95, k)
{
  ellipse1(a, b,, phi, k) * qf(level, df1, df2)
}
