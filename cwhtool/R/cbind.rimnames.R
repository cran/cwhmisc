cbind.colnames <- function(add,to=NULL,deparse.level = 1)  {
  if (is.null(add)) to
  else if (is.null(to)) {
    first <- get(add[1])
    to    <- cbind(first,deparse.level)
    mode(to)  <- mode(first)
    class(to) <- class(first)
    res <- cbind.colnames(add[-1],to,deparse.level)[,-2] # kill col 2=deparse ??
    dimnames(res)[[2]][1] <- add[1]
    res
  }
  else {
    res <- to;  dimn2 <- dimnames(to)[[2]]
    for (ii in add) {
      res   <- cbind.data.frame(res,get(ii),deparse.level = deparse.level)
      dimn2 <- c(dimn2,ii)
    }
    dimnames(res)[[2]] <- dimn2
    res
  }
}

