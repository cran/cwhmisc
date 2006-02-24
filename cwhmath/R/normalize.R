normalize <- function(x) {
  x/rep(sqrt(drop(apply(x,2, function(x) sum(x^2)))),rep(nrow(x),ncol(x)))
}
