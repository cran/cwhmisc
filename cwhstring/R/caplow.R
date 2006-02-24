capply <- function(str, ff) {
  sapply(lapply(strsplit(str, NULL), ff), paste, collapse="")
}

cap <- function(char) {
  # change lower letters to upper, others leave unchanged
  if (any(ind <- letters==char)) LETTERS[ind]
  else char
}

capitalize <- function(str) { # vector of words
  ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),cap),collapse="")
  capply(str,ff)
}
       
lower <- function(char) {
  # change upper letters to lower, others leave unchanged
  if (any(ind <- LETTERS==char)) letters[ind]
  else char
}

lowerize <- function(str) {
  ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),lower),collapse="")
  capply(str,ff)
}

"CapLeading" <- function(str) {
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
    

