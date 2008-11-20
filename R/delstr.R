"delstr" <- function(str,del) {
  ## delete the character sequence del from string str, if contained
  if ((nchar(str)<nchar(del)) | (nchar(del)==0)) str
  else if (str==del) ""
  else {
    n1 <- nchar(del)-1
    ns <- nchar(str)
    ss <- substring(str,1:ns,c((1+n1):ns,rep(ns,n1)))
    ind <- seq(1:ns)[ss == del]
    if (is.na(ind > 0)) # test if ind exists
      str
    else {
      for (i in length(ind):1) { # backwards !!
        ss <- ss[-(ind[i]:(ind[i]+n1))]
      }
      paste(substring(ss,1,1),collapse="")
    }
  }
}

