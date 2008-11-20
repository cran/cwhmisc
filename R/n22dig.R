"n22dig" <- function(x, symm = TRUE) {
  n22dig0 <- function(y) { ifelse(y == "00", " I", y) }
  y <- format(round(100 * x))
  if(nchar(y[1] == 3)) y <- substring(y, 2)
  if (is.matrix(x))
    y <- ifelse((row(x) < col(x)) & symm, " ", n22dig0(y))
  else
    y <- n22dig0(x)
  y
}
