frac <- function(x,d) {  # fractional part
  res <- abs(x-trunc(x))
  if (!missing(d)) res <- round(10^d*res)
  res
}
