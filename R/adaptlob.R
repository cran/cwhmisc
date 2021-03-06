.adaptsimstp <- function(f, term, a, b, fa, fm, fb, is, trace, ...) {
#.adaptsimstp  Recursive function used by adaptsim.
#
#   q <- .adaptsimstp("f",a,b,fa,fm,fb,is,trace) tries to
#   approximate the integral of f(x) from a to b to
#   an appropriate relative error. The argument "f" is
#   a string containing the name of f. The remaining
#   arguments are generated by adaptsim or by recursion.
#
#   See also adaptsim.
#
#   Walter Gander, 08/03/98
  h <- (b - a) / 4
  m <- (a + b) / 2
  x <- c(a + h, b - h)
  y <- f(x, ...)
  fml <- y[1]
  fmr <- y[2]
  i1  <- h / 1.5 * (fa + 4 * fm + fb)
  i2  <- h / 3 * (fa + 4 * (fml + fmr) + 2 * fm + fb)
  i1  <- (16 * i2 - i1) / 15
  if((is + (i1 - i2) == is) | (m <= a) | (b <= m)) {
    if( ( (m <= a) | (b <= m)) & (!term)) {
       warning("See description.")
       term <- TRUE
    }
    Q <- list(Q = i1, term = term)
    if(trace) cat(a, b - a, Q$Q, Q$term, "\n")
  }
  else  {
    Q1 <- Recall(f, term, a, m, fa, fml, fm, is, trace, ...)
    Q2 <- Recall(f, term, m, b, fm, fmr, fb, is, trace, ...)
    Q  <- list(Q = Q1$Q + Q2$Q, term = Q1$term & Q2$term)
  }
  return(Q)
}

adaptsim <- function(f, a, b, tol = .Machine$double.eps, trace = FALSE, ...) {
#adaptsim  Numerically evaluate integral using adaptive
#   Simpson rule.
#
#   adaptsim(f,a,b) approximates the integral of
#   f(x) from A to B to machine precision. The
#   function f must return a vector of output values if
#   given a vector of input values.
#
#   adaptsim(f,a,b,tol) integrates to a relative
#   error of TOL.
#
#   adaptsim(f,a,b,tol,trace) displays the left
#   end point of the current interval, the interval
#   length, and the partial integral.
#
#   adaptsim(f,a,b,tol,trace,p1,p2,...) allows
#   coefficients p1, ... to be passed directly to the
#   function f:  G <- f(x,p1,p2,...). To use default values
#   for tol or trace, one may pass the empty matrix ([]).
#
#   See also adaptsimstp.
#
#   Walter Gander, 08/03/98
#   Reference: Gander, Computermathematik, Birkhaeuser, 1992.

  term <- FALSE
  tol <- max(tol, .Machine$double.eps)
  x <- c(a, (a + b) / 2, b)
  y <- f(x, ...)
  fa <- y[1]
  fm <- y[2]
  fb <- y[3]
  yy <- f(a + c(0.9501, 0.2311, 0.6068, 0.4860, 0.8913) * (b - a), ...)
  is <- (b - a) / 8 * (sum(y) + sum(yy))
  if(is == 0) is <- b - a
  is <- is * tol / .Machine$double.eps
  Q <- .adaptsimstp(f, term, a, b, fa, fm, fb, is, trace, ...)
  if(trace) catn(a, b - a, Q$Q, Q$term)
  return(Q)
}  # end adaptsim

adaptlob <- function (f, a, b, tol = .Machine$double.eps, trace = FALSE, ...) {
#   See adaptlobstp.
#   Walter Gautschi, 08/03/98
#   Reference: Gander, Computermathematik, Birkhaeuser, 1992.
  term <- FALSE
  tol <- max(tol, .Machine$double.eps)
  m <- (a + b) / 2
  h <- (b - a) / 2
  alpha <- sqrt(2 / 3)
  beta <- 1 / sqrt(5)
  x1 <- 0.942882415695480
  x2 <- 0.641853342345781
  x3 <- 0.236383199662150
  x <- c(a, m - x1 * h, m - alpha * h, m - x2 * h, m - beta * h, m - x3 * h, m,
         m + x3 * h, m + beta * h, m + x2 * h, m + alpha * h, m + x1 * h, b)
  y <- f(x, ...)
  fa <- y[1]
  fb <- y[13]
  i2 <- (h / 6) * (y[1] + y[13] + 5 * (y[5] + y[9]))
  i1 <- (h / 1470) * (77 * (y[1] + y[13]) + 432 * (y[3] + y[11]) + 625 * (y[5]
          + y[9]) + 672 * y[7])
  is <- h * (.0158271919734802 * (y[1] + y[13]) + 0.0942738402188500
      * (y[2] + y[12]) + 0.155071987336585 * (y[3] + y[11]) +
     0.188821573960182 * (y[4] + y[10]) + 0.199773405226859
      * (y[5] + y[9]) + 0.224926465333340 * (y[6] + y[8])
      + 0.242611071901408 * y[7])
  erri1 <- abs(i1 - is)
  erri2 <- abs(i2 - is)
  R <- 1
  if(erri2 != 0) R <- erri1 / erri2
  if(R > 0 & R < 1) tol <- tol / R
  is <- signp(is) * abs(is) * tol / .Machine$double.eps
  if(is == 0) is <- b - a
  Q <- .adaptlobstp(f, term, a, b, fa, fb, is, trace, ...)
    if(trace) cat(a, b - a, Q$Q, Q$term, "\n")
  return(Q)
}  # end adaptlob

.adaptlobstp <- function(f, term, a, b, fa, fb, is, trace, ...) {
#   Recursive function used by adaptlob.
#
#   Q  <-  .adaptlobstp("f",a,b,fa,fb,is,trace) tries to
#   approximate the integral of f(x) from a to b to
#   an appropriate relative error. The remaining
#   arguments are generated by adaptlob or by recursion.
#
#   See also ADAPTLOB.
#   Walter Gautschi, 08/03/98
  h <- (b - a) / 2
  m <- (a + b) / 2
  alpha <- sqrt(2 / 3)
  beta  <- 1 / sqrt(5)
  mll <- m - alpha * h
  ml  <- m - beta * h
  mr  <- m + beta * h
  mrr <- m + alpha * h
  x   <- c(mll, ml, m, mr, mrr)
  y   <- f(x, ...)
  fmll <- y[1]
  fml  <- y[2]
  fm   <- y[3]
  fmr  <- y[4]
  fmrr <- y[5]
  i1 <- (h / 1470) * (77 * (fa + fb) + 432 * (fmll + fmrr) +
        625 * (fml + fmr) + 672 * fm)
  i2 <- (h / 6) * (fa + fb + 5 * (fml + fmr))
  if( (is + (i1 - i2) == is) | (m <= a) | (b <= m)) {
    if(( (m <= a) | (b <= m) ) & (!term) ) {
       warning("\n  See description.")
       term <- TRUE
    }
    Q <- list(Q = i1, term = term)
    if(trace) cat(a, b - a, Q$Q, Q$term, "\n")
  } else {
    Q1 <- Recall(f, term, a, mll, fa, fmll, is, trace, ...)
    Q2 <- Recall(f, term, mll, ml, fmll, fml, is, trace, ...)
    Q3 <- Recall(f, term, ml, m, fml, fm, is, trace, ...)
    Q4 <- Recall(f, term, m, mr, fm, fmr, is, trace, ...)
    Q5 <- Recall(f, term, mr, mrr, fmr, fmrr, is, trace, ...)
    Q6 <- Recall(f, term, mrr, b, fmrr, fb, is, trace, ...)
    Q  <- list(Q = Q1$Q + Q2$Q + Q3$Q + Q4$Q + Q5$Q + Q6$Q,
      term = Q1$term & Q2$term & Q3$term & Q4$term & Q5$term & Q6$term)
  }
    return(Q)
}  # end .adaptlobstp
