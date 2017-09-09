str2dig <- function( str ) {
  ops <- options(warn = -1)
  on.exit(options(ops))
  as.numeric( unlist(strsplit(str,NULL)) )
} # str2dig

NdM <- function( x, B=10 ) max( Nd( x, B ))
  
int2ASCII <- function( n ) {
  ASCII[n %% 256];
} # int2ASCII

int2B <- function( n, B=10, space, plus=lead, lead="",just=c("left","right","center","none") ) {
    just <- match.arg(just)
  res <- r2B( n, B, plus=plus, lead=lead, just=just)
  names(res) <- names( n )
  return ( res )
} # int2B

int2Oct <- function( n ) {
  int2B( n,8)
} # int2Oct

int2Hex <- function( n ) {
  int2B( n,16)
} # int2Hex

strRound <- function( str, digits = getOption("digits"), B=10) {
  if ( B==10 ) { res <- round( as.numeric( str ), digits )
  } else { # 2016-05-25, 13:08:26
    stopifnot(is.character(str))
    str <- sub(" ", "", str)
    x1 <- substring( str,1,1)
    str <- substring( str,2)
    Sign <- ifelse ( x1 == "-", -1, 1 )
    ddot <- cpos( str, "." )
    if ( is.na(ddot) ) ddot <- nchar(str) + 1
    nddig <- nchar(str) - ddot
#    catE("ddot")
    if ( is.na(ddot) ) nddig <- 0
    str <- sub("\\.","",str)
    D <- as.numeric( unlist( strsplit(str,"") ) )  #CF[1:(ddot + digits)]
    E <- D[1:min(ddot-1+digits, length(D))]
    sum <- 0
    for (kk in  seq_along(E) ) {
      sum <-  sum*B + E[kk]
    } # kk
#    x <- abs(x) + 0.5*B^(-digits)
    F <- B^min( digits, nddig )
    roun <- ddot+digits <= length(D)
    res <- Sign*trunc( sum + (if(roun)D[ddot+digits]/B else 0) + 1/2 ) / F
  } #else
  return( res )
} # strRound
