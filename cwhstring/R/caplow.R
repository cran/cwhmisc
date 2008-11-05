capply <- function(str, ff,...) {
  x <- strsplit(str, NULL)
  y <- lapply(x, ff,...)
  sapply(y, paste, collapse="")
} ## 2008-09-29, intermediate results introduced

cap <- function(char) {
  # change lower letters to upper, others leave unchanged
  if (any(ind <- letters==char)) LETTERS[ind]
  else char
}

capitalize <- function(str) { # vector of words
  capply(str,caplow.ff,cap)
}
       
lower <- function(char) {
  # change upper letters to lower, others leave unchanged
  if (any(ind <- LETTERS==char)) letters[ind]
  else char
}

lowerize <- function(str) {
  capply(str,caplow.ff,lower)
}

CapLeading <- function(str) {
  ff <- function(x) {r <- x; r[1]<-cap(x[1]); r}
  capply(str,ff)
}

#cap("f")
#cap("R")
#capitalize(c("TruE","faLSe"))
#capitalize(c("faLSe","TruE"))
#lower("f")
#lower("R")
#lowerize("TruE")
#lowerize("faLSe")
    

