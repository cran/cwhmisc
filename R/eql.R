eql <- function(x, y) {
  if(length(x) > length(y)) {
    y <- rep(y, , length(x))
  } else {
    x <- rep(x, , length(y))
  }
  rel2 <- ifelse(is.nan(x), is.nan(y), ifelse(is.nan(y), FALSE, x == y))
  return(rel2)  
}
