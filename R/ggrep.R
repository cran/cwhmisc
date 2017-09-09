grepnot <- function(str,x,value=TRUE) {
  c(grep( str,x, value=value, invert=FALSE),"|\\|",      grep(str,x,value=value,invert= TRUE) )
} # grepnot

ggrep <- function ( opt= "inr",str, dir= "/Users/hoffmannc/R/", pkg="", split=FALSE, lines=10, out=FALSE) {
  ops <- options(warn = -1)
  on.exit(options(ops))
  Split <- if (split) unique(unlist(strsplit(str,NULL))) else str
  filedir <- if (pkg != "") paste0( dir,"/",pkg) else dir
  if (out) catE("filedir")
  fileex <- file.exists( filedir )
  if( !fileex ) catn( filedir, " may be a directory")
  outs <- paste0("grep -",opt," '",Split,"' ",filedir,collapse="")
  if (out) catE("outs")
  res <- system ( outs, intern = TRUE )
  res <- res[1:min(lines,length( res ))]
  if (out) catE("res")
  return ( res )
} # ggrep


countChar <- function( str, dir="/Users/hoffmannc/R/", pkg="",split=FALSE, out=FALSE) {
  SF <- function(S) ggrep(opt="cr",S,dir=dir,pkg=pkg,split=split,out=out)
  Split <- if (split) unlist(strsplit(str,NULL)) else str
  sa <- sapply( Split,SF )
  xx <- unlist (
    sapply(sa,function(x) sub( paste0( dir,pkg ),"..",x), simplify=FALSE ) )
  x <- sapply(xx,function(y) sub(":",": ",y) )
#  x <- sapply(xx,function(y) print(sub(":",": ",y),quote=FALSE) )
  names(x) <- NULL
  return ( x )
} # countChar
