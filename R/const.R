cE      <-  2.718281828459045235     ## e
cLOG2E  <-  1.442695040888963407     ## log(basis 2)  of e
cLOG10E <-  0.4342944819032518277    ## log(basis 10)  of e
cLN2    <-  0.6931471805599453094    ## log(basis e)  of 2
cLN10   <-  2.302585092994045684     ## log(basis e)  of 10
c2PI    <-  6.283185307179586477     ## 2pi, 2p
cPIBY2  <-  1.570796326794896619     ## pi/2, p/2
cPIBY4  <-  0.7853981633974483096    ## pi/4, p/4
c1BYPI  <-  0.3183098861837906715    ## 1/pi, 1/p
c2BYPI  <-  0.6366197723675813431    ## 2/pi, 2/p
c1BYSQRTPI <-  0.5641895835477562869    ## 1/sqrt(pi), 1/sqrt(p)
c2BYSQRTPI <-  1.12837917095512574     ## 2/sqrt(pi), 2/sqrt(p)
cSQRT2	   <-  1.414213562373095049    ## sqrt(2)
cSQRT2BY2  <-  0.7071067811865475244    ## sqrt(2)/2

cMAXREALBY3Q <- .Machine$double.xmax^0.75
cMAXREALBY38 <- sqrt(cMAXREALBY3Q) 

GreatestIntAsRealF <- function( ) {
  ## Find the greatest integer K which is distiguishable from (K+1), both represented as real
  q <- 1.0;
  while (p != q) { q <- q+q;  p <- q+one };
  return( q/2.0)
}  ## END  GreatestIntAsRealF;

spacC <- "  "  ## extra space in indexLine and charMat

ASCII <- c(NA, sapply(1:255, function(i) parse(text=paste("\"\\", structure(i,class="octmode"), "\"", sep=""))[[1]]));

HexDig <- c('0','1','2','3','4','5','6','7','8','9',LETTERS[1:6])

PRIMES <- c( 2, 3, 5, 7, 11, 13, 17, 19, 23, 29,  31, 37, 41, 43, 47,  53, 59, 61, 67, 71,  73, 79, 83, 89, 97,  101, 103, 107, 109, 113,  127, 131, 137, 139, 149,  151, 157, 163, 167, 173,  179, 181, 191, 193, 197,  199, 211, 223, 227, 229,  233, 239, 241, 251, 257,  263, 269 , 271, 277, 281,  283, 293, 307, 311, 313,  317, 331, 337, 347, 349,  353, 359, 367, 373, 379,  383, 389, 397, 401, 409,  419, 421, 431, 433, 439,  443, 449, 457, 461, 463,  467, 479, 487, 491, 499,  503, 509, 521, 523, 541,  547, 557, 563, 569, 571,  577, 587, 593, 599, 601,  607, 613, 617, 619, 631)
