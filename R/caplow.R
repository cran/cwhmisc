capply <- function(str, ff, ...) {
  x <- strsplit(str, NULL)
  y <- lapply(x, ff, ...)
  return(sapply(y, paste, collapse = ""))
} # capply

cap <- function(char) {
  if (any(ind <- letters == char)) LETTERS[ind]
  else char
}

.cl <- function(str, ff) {
  x <- strsplit(str, NULL)
  y <- unlist(x)
  z <- lapply(y, ff)
  paste(z, collapse = "")
}

capitalize <- function(str) {
  capply(str, .cl, cap)
}
       
lower <- function(char) {
  if (any(ind <- LETTERS == char)) letters[ind]
  else char
}

lowerize <- function(str) {
  capply(str, .cl, lower)
}

CapLeading <- function(str) {
  ff <- function(x) {
    r <- x
    r[1] <- cap(x[1])
    return(r)
  }
  capply(str, ff)
}

strReverse <- function(str)  capply( str, rev)
