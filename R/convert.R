allDigits <- function(str) {
  k <- length(str)
  result <- logical(k)
  for(i in 1:k) {
    st <- str[i]
    ls <- nchar(st)
    ex <- strsplit(st, NULL)
    result[i] <- all(match(ex,c('0','1','2','3','4','5','6','7','8','9'),nomatch=0)>0)
  }
  result
}

intToASCII <- function(i) {
  ASCII[i %% 256];
}

intToBase <- function(i,base=2) {
  stopifnot(2 <= base & base <= 16)
  res <- HexDig[i %% base + 1]
  i <- i - i %% base
  while (i > 0) {
    i <- i %/% base
    res <- paste(HexDig[i %% base + 1],res,sep="")
    i <- i - i %% base
  }
  res
}

intToOct <- function(i) {
  intToBase(i,8)
}

intToHex <- function(i) {
  intToBase(i,16)
}

contfrac <- function( x, depth = 13, f=floor ) {
  ## from Mathematics 5335 Fall 2009
  ## The Euclidean Algorithm and Continued Fractions
    fac <- 10^(-3) ## to prevent Inf caused by division by nearly zero
    xi  <- x
    ai <- a  <- f( xi );
    ii <- 1
    while (
           (1.0 + abs(ai-xi)*fac != 1.0) && (ii <= depth)
           ) {
      (xi <- 1.0/( xi - ai ))
      (ai <- f(xi))
      (a  <- c( a, ai) )
      ii <- ii+1
    }
    return( a )
}  # end contfrac

evalcfr <- function( cf ) {
  res <- cf[length(cf)]
  for (ii in rev(seqm(1,length(cf)-1)) ) res <-cf[ii] + 1.0/res
  return( res )
}  # end evalcfr

toFrac <- function( x, depth) {
  sig  <- signp(x);
  x    <- abs(x);
  minr <- 1.0/.Machine$integer.max;
  n    <- 1;
  num1 <- 1.0;      den1 <- 0.0;
  num  <- int(x);   den  <- 1.0;
  val1 <- num/den;  val2 <- 0.0;
  dif2 <- .Machine$integer.max;
  dif1 <- abs(val1-val2);
  r    <- x - num;
  while ((n < depth) & (r > cMAXREALBY38) < (r >= minr)) {
#  while ((n < depth) & (dif1 < dif2) & (r >= minr)) {
    ## avoid division by 0 *)
   n <- n+1;
   onebyr <- 1/r + 8.*minr;  # force some rounding
   b <- int(onebyr);
   r <- onebyr - b;
   num2 <- num1;  den2 <- den1;
   num1 <- num ;  den1 <- den;
   num  <- num1*b + num2;
   den  <- den1*b + den2;
   val2 <- val1;
   val1 <- num/den;
   dif2 <- dif1;
   dif1 <- abs(val1-val2);
  } #END;  ##ELIHW*)
  return(list(num=sig*num,den=den))
} # end toFrac

str2dig <- function( str ) {
    as.numeric( unlist(strsplit(str,NULL)) )
}

int <- function(x) {
  as.integer(trunc(x))
}

xToBase <- function( x, base=2 ) {
  e <- trunc(log(abs(x),base=base));  a <- x/base^e
  if ( abs(a) < 1.0 ) { e <- e-1.0;   a <- a*base
  } else { if (abs(a) > base ){ e <- e+1.0; a <- a/base }
  }
  return(list(a=a,e=e))
}

##  -------------------------------------------------------------
		 ## Conversion functions *)
Hd <- function( h, m, s ) {
  return ( s/60.0 + m )/60.0 + h
} ## end  Hd

Hms <- function( hd ) {
  h <- int( hd )
  s <- hd - h;  s <- s*60.0
  m <- int( s )
  s <- 60.0 * (s - m)
  return (list(h=h,m=m,s=s))      
} ## end  Hms

Hdms <- function( hd ) {
  r <- Hms( hd )
  return ((r$s/100.0 + r$m)/100.0 + r$h)
} ## end  Hdms

Hmsd <- function( hms ) {
  h <- int( hms )
  s <- hms - h;  s <- s*100.0
  m <- int( s )
  s <- 100.0 * (s - m)
  return ((s/60.0 + m )/60.0 + h)
} ## end  Hmsd

		 ##* Conversion functions *)
Degree <- function( radian ) {
  return  (radian/pi*oneeigthy)
} ## end  Degree

Radian <- function( degree ) {
  return  (degree*pi/oneeigthy)
} ## end  Radian

ReduceArc <- function( U, ref ) {
  return  (Mod( U + ref*0.5, ref ) - ref*0.5)
} ## end  ReduceArc

ReduceArc2 <- function(U, V, ref ) {
  n <- if (U <= V) floor( U/ref )  else floor( V/ref )
  z    <- n*ref
  return(list(U=U-z,V=V-z))
} ## end  ReduceArc2

ToPolar <- function( x, y ) {	##* Rectangular -> polar coordinates *)
  ## combinations for (x,y):
  ## 1. (complex,missing)
  ## 2. (list(r=,p=),missing) is already polar
  ## 3. (list(x=,y=),missing)
  ## 4. (real, imaginary)
  ## 5. (real, missing)
  if (mode(x) != "complex") { # cases 2, ...
    if (missing(y)) { # cases 2, 3, 5
      if (is.list(x)) {
        if (names(x)[1]=="r") {
          return (x)   # case 2.
        } else { x <- complex(real=x$x,imaginary=x$y) # 3.
        }
      } else {
        return ( complex(real=x,imaginary=0) ) # 5
      }
    } else { # case 4.
      x <- complex(real=x,imaginary=y)
    }
  }
  return (list(r=Mod(x),p=Arg(x)))
} ## end  ToPolar

ToRect <- function( r, p ) {  ## combinations for (r,p):
  ## 1. (complex,missing)
  ## 2. (list(r=,p=),missing)
  ## 3. (list(x=,y=),missing) is already rect.
  ## 4. (modulus, argument)
  ## 5. (modulus, missing)
  if (mode(r) != "complex") { # cases 2, ...
    if (missing(p)) { # cases 2, 3, 5
      if (is.list(r)) {
        if (names(r)[1]=="x") {
          return (r)   # case 3.
        } else { r <- complex(modulus=r$r,argument=r$p) # 2.
        }
      } else {
        return ( complex(modulus=r,argument=0) ) # 5
      }
    } else { # case 4.
      r <- complex(modulus=r,argument=p)
    }
  }
  return (list(x=Re(r),y=Im(r)))
}  ## end  ToRect

Hd <- function( h, m, s ) {
  return (( s/60.0 + m )/60.0 + h)
} ## end  Hd

Hms <- function( hd ) {
  h <- floor( hd )
  s <- hd - h;  s <- s*60.0
  m <- floor( s )
  s <- 60.0 * (s - m)
  return (list(h=h,m=m,s=s))      
} ## end  Hms

Hdms <- function( hd ) {
  r <- Hms( hd )
  return ((r$s/100.0 + r$m)/100.0 + r$h)
} ## end  Hdms

Hmsd <- function( hms ) {
  h <- int( hms )
  s <- hms - h;  s <- s*100.0
  m <- int( s )
  s <- 100.0 * (s - m)
  return ((s/60.0 + m )/60.0 + h)
} ## end  Hmsd

		 ##* Conversion functions *)
Degree <- function( radian ) {
  return  (radian/pi*180.0)
} ## end  Degree

Radian <- function( degree ) {
  return  (degree*pi/180.0)
} ## end  Radian

ReduceArc <- function( U, ref ) {
  return  (( U + ref*0.5)%%ref - ref*0.5)
} ## end  ReduceArc
