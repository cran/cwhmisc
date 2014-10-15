ellipse1 <- function(k, m, a, b, phi, mlt = 1.0 )
{
  alpha <- seq(0, by = (2 * pi)/k, length = k+1)   # define angles, overlapping
  Z <- cbind(a*cos(alpha), b*sin(alpha))     # points on unit circle, (m+1)*2
  R <- rotL(phi,1,2)[-3,-3] # 2*2
  res <- m + mlt * Z %*% R
  return( res )
}  # end of ellipse

conf.ellipse <- function(k, m, a, b, phi, df1, df2, level = 0.95)
{
  ellipse1(k, m, a, b, phi, qf(level, df1, df2) ) 
}
