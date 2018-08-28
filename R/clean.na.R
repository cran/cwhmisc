clean.na <- function (x, margin, drop = FALSE)  {
  ind <- apply(x, margin, function(xx) all(!is.na(xx)))
  if (margin == 1)   x[ind, , drop = drop]
  else x[, ind, drop = drop]
}
