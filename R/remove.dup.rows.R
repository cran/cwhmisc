remove.dup.rows <- function(dfr) {
  o <- do.call("order", dfr)
  all.dup <- do.call("pmin", lapply(dfr[o, ], function(x) eql(x, c(x[-1], NA))))
  all.dup[o] <- all.dup
  dfr[!all.dup, ]
}
