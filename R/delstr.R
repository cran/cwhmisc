delstr <- function(str, del) {
  if ((nchar(str) < nchar(del)) | (nchar(del) == 0)) {
    return(str)
  } else if (str ==  del) {
      return("")
    } else {
    n1 <- nchar(del) - 1
    ns <- nchar(str)
    ss <- substring(str, 1:(ns-n1), c((1 + n1):ns))
    ind <- seq(1:ns)[ss == del]
    if (length(ind) == 0)
      return(str)
    else {
      for (i in length(ind):1) {
        ss <- ss[-(ind[i]:(ind[i]+n1))]
      }
      return(paste(substring(ss, 1, 1), collapse = ""))
    }
  }
}
