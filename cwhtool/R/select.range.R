"select.range" <- function (groupvec, min, max, data) {
  if (nargs() > 3) {
    min.cond <- groupvec >= min
    max.cond <- groupvec < max
    cond <- min.cond & max.cond
#    selected <- na.remove(ifelse(cond, data, NA))
#    invisible(selected)
    data[cond]
  }
  else cat("Usage: select.range(groupvec,min,max,datavec)\n")
}
