formatFix <- function(x, after = 2, before = 1, extend = TRUE) {
  xx <- as.vector(x)
  stripform <- function(x, afterS, len) {
    st <- format(x, digits = min(max(1, afterS), 22), trim = TRUE,
            nsmall = afterS)
    difflen <- nchar(st) - len
    while (difflen < 0) {
      st <- paste(" ", st, sep = "")
      difflen <- difflen + 1
    }
    while ((difflen > 0) & (substring(st, 1, 1) == " ")) {
      st <- substring(st, 2)
      difflen <- difflen - 1
    }
    if (difflen) paste(rep("*", len), collapse = "")
    else st
  }  ### stripform
  
  maxA  <- 1.0e8
  aft <- max(after, 0)
  withdot <- aft > 0
  toobig  <- ifelse(is.na(xx), TRUE, abs(xx) >= maxA)
  decim   <- pmax(floor(log10(abs(xx)) * (1 + .Machine$double.eps)), 0)
  reqbef  <- ifelse(is.na(xx), 2, pmax(decim, 0) + as.numeric(xx < 0) + 1)
  placesbefore <- ifelse(is.na(xx), 2, ifelse(rep(extend, length(xx)),
                 decim + 2, pmin(before, reqbef)))
  placesbefore[toobig] <- 0
  xx     <- round(abs(xx) * 10 ^ aft)  #  treat as integer
  before <- max(before, placesbefore)
  filldot <- reqbef > before
  regular <- !filldot & !toobig
  len <- mlen <- before + aft + 1
  if (!withdot) mlen <- mlen - 1
  if (extend & any(toobig)) {
    ncc <- max(nchar(format(xx[toobig], digits = min(max(1, aft),
             22)))) - mlen
    if (ncc  > 0) {
      mlen <- mlen + ncc
      len <- len + ncc
    }
  }
  strm <- matrix("*", mlen, length(x))
  strm[, regular] <- " "
  if (any(regular)) {
    kk <- 1
    while (kk <= aft) {
      strm[len - kk + 1, regular] <- xx[regular] %% 10
      xx[regular] <- xx[regular] %/% 10
      kk <- kk + 1
    }
    if (withdot) strm[len - kk + 1, regular] <- "."
    while (max(xx[regular]) > 0 | kk == aft + 1) {
      strm[len - kk, regular] <- ifelse(xx[regular] > 0 | kk == aft + 1,
           xx[regular] %% 10, strm[len - kk, regular])
      xx[regular] <- xx[regular] %/% 10
      kk <- kk + 1
    }
    strm[cbind(len - aft - placesbefore, seq(ncol(strm)))
         [regular & (x < 0), ]] <- "-"
  }
  res <- apply(strm, 2, paste, collapse = "")
  if (any(toobig)) res[toobig] <- sapply(xx[toobig], stripform,aft, mlen)
  names(res) <- names(x)
  if (is.array(x)) dim(res) <- dim(x)
  return (res)
}
