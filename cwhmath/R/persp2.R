persp2 <- function(d,axis,r) {
  # perspective along axis axi from point (axi=r,0,0)
  xy <- setdiff(1:3,axis)
  rl <- r/(r-d[,axis])
  res <- d
  res[,xy[1]] <- res[,xy[1]]*rl
  res[,xy[2]] <- res[,xy[2]]*rl
  res
}
