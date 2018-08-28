chsvd <- function(s) return(s$u %*% diag(s$d) %*% t(s$v))

divmod <- function(i,n) return(c(i %/% n, i %% n))

divmodL <- function(i,n) return(list(d = i %/% n, m = i %% n))

dsm <-  function(x, w) {
   dm <- divmod( x, sum(w) )
   sm <- submod( dm[2], cumsum(w) )
   res <- c( dm[1], sm )
   return(res)
 } # dsm

equal <- function(x, y)  x == y

equalFuzzy <- function(x, y, prec=8 * .Machine$double.eps, rel=TRUE) {
  return(abs(x - y) <= prec * (ifelse(rel, abs(x) + abs(y), 1.0)))
} # equalFuzzy

exch <- function(x,L,R) {
  indL <- which(x == L)
  indR <- which(x == R)
  x[indL] <- R
  x[indR] <- L
  return (x)
} # exch

frac <- function(x,d) {  # fractional part
  res <- abs(x-trunc(x))
  if (!missing(d)) res <- round(10 ^ d * res)
  res
} # frac

int  <- function (x) as.integer(trunc(x))

inrange <- function(x,y) (!is.na(x) && min(y) <= x & x <= max(y))

Ko <- function(z) (z - 1i) / (z + 1i)

Km <- function(z) (-1i) * (z + 1) / (z - 1) # inverse of K

last <- function(x) rev(x)[1] # last element

LE <- function(x) length(x)

loop.vp <- function(ve, overlap=1) {
       # add first element after last
  if(is.matrix(ve)) {
    res <- cbind(ve, ve[, overlap ])
  } else {
    res <- c(ve, ve[overlap])
  }
  return(res)   
} # loop.vp

LS <- function() .Last.value

lV <- function(x) as.vector(sqrt(x %*% x))

mod <- function(x, y) x %% y

modR <- function(x, y) return  (x - (floor(x / y) * y))

modS <- function(x, y) return  (x - (trunc(x / y) * y))

norm2 <- function(x) {
  s <- 0
  for(kk in seq_along(x)) {
    s <- pythag( s, x[kk] )[1]
  } # kk
  return(s)
} # norm2

one  <- function(x) 1.0

onebyx <- function(x) 1.0 / x
  
powr <- function(a, x) a ^ x

pythag <- function(a, b) {
       # Pythagorean sum
   p <- max(abs(a), abs(b))
   q <- min(abs(a), abs(b))
   while (TRUE) {
     r <- (q / p) ^ 2
     if (r + 4 == 4) break
     s <- r / ( 4 + r )
     p <- p + 2 * s * p
     q <- s * q
   }
   return(c(p, r))
 } # pythag

quotmean <- function(x,y) mean(x, na.rm = TRUE) / mean(y, na.rm = TRUE)

safeDiv <- function(num, den) {
  q <- ifelse (num == 0 & den == 0, 1,  num / den)
  return( ifelse (is.infinite(q), c3Q, q) )
} # safeDiv

signp <- function(x) return(ifelse( is.complex(x), NA, ifelse( is.na(x) |
       !is.finite(x) | x>=0 , 1, -1)))

sqr <- function( x ) x ^ 2

solveQeq <- function(a,b,c) { # solve ax^2 + bx + c = 0 for x
  stopifnot(!missing(c) || length(a) == 3)
  if (missing(c)) {
    c <- a[3]
    b <- a[2]
    a <- a[1]
  }
  c1 <- c; c2 <- b; c3 <- a[1]
  p <- b / a; q <- c / a
  if (a == 0) {
    x1 <- -c / b
    res <- c(x1, x1)
  } else {
    p2 <- p * 0.5
    dis <- sqrt(p2 ^ 2 - q)
    x1 <- -p2 + dis
    x2 <- -p2 - dis
    res <- c(x1,x2)
  }
  return(res)
} # solveQeq

sqr <- function( x ) x ^ 2

sqrtH <- function( x ) {
  stopifnot( x >= 0 )
  xn <- rapply(normalize(x,2), function(y) y, how = "unlist")
  x0 <- xn[1] * 2 ^ (xn[2] / 2)
  names( x0 ) <- ""
  p <- x
  while ( TRUE ) {
    xq <- x0 ^ 2
    x1 <- x0 * (3 * p +  xq) / (p + 3 * xq)
      if(abs(x1 - x0) + 4 == 4) break
    x0 <- x1
  }# while
  return(x1)
} # sqrtH

submod <- function(x, v) {
  if (x <= 0) return ( c(0, 0) )
  v <- c(0, v)
  ii <- sum( v < x  )
  return(c( ii - 1, x - v[ii]))
}  # submod

zero <- function( x ) 0.0
