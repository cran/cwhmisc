pad <- function(str, space, just = c("right", "left", "center", "none"),
                  with=" ") {
  res  <- str
  just <- match.arg(just)
  if (just == "none")
    res  <- NA else {
    if (with == "") with <- " "
    for ( kk in seq_along(str) ) {
      free <- space - nchar(str[kk])
      jj <- ceiling(free / nchar(with))
      fill <- if ( free > 0 )
        substring(paste(rep.int(with, jj), collapse = ""),1,free) else ""
      res[kk] <- if (free <= 0) {
           str[kk]
        } else {
          switch(just,
            "left"  = paste0(fill,str[kk]),
            "right" = paste0(str[kk],fill),
            "center"= {
                hf <- (free + 1) %/% 2
                paste0(substring(fill, 1, hf), str[kk],
                       substring(fill, 1+hf,free))
              }              
            )
          }
      } # kk
    } # else NA
  return(res)
}

insstr <- function( str, ins, point=nchar(str) ) {
  res <- str
  pp <- rep(point,length=length(str))
  for (ii in seq_along(str)) {
    s1 <- paste00(substring(str[ii],1,pp[ii]) )
    s2 <- paste00(c(ins, substring(str[ii],pp[ii]+1)) )
    res[ii] <- paste00(s1,s2)
  } # ii
  return( res )     
}

justify <- function(str, space, just = c("right", "left", "center", "none"), with=" ") {
  loc  <- match.arg(just)
  loc1 <- exch( loc, "right", "left" )
  return( pad( str, space, loc1, with ) )
}
