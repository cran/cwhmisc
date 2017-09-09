parsecheck <- function(str="/Users/hoffmannc/R/test0/R")
    for (f in list.files(str, full.names=TRUE)) parse(f)
