is.constant <-  function(x) {
  if (is.factor(x)) {
    res <- (length(attributes(x)$levels) == 1) && !any(is.na(as.character(x)))
  } else {
    res <- is.numeric(x) && !any(is.na(x)) && identical(min(x), max(x))
  }
  return(res)
}
