### Rational Transfer Function objects for S-PLUS.
### Author:  Henrik Aalborg Nielsen, IMM, DTU (han@imm.dtu.dk)

rtf <- function(A=1, B=1, delay=0, unit.sg=TRUE, stability.check=TRUE)
{
  ## Creates and checks a rational transfer-function object
  ##
  ## y_t = H(q) x_t, where H(q) = q^-delay B(q^-1) / A(q^-1)
  ##
  ## If unit.sq is TRUERUE (default) the coefs. of B(q^-1) is multiplied
  ## with a factor making the stationary gain one.
  ##
  ## A(q^-1) = 1   - a_1 q^-1 - ... - a_na q^-na
  ## B(q^-1) = b_0 + b_1 q^-1 + ... + b_nb q^-nb
  ##
  ## Note that '-' is used in A() and '+' in B(), these are specified as:
  ##
  ## A = c(1,   a_1, .... , a_na)
  ## b = c(b_0, b_1, .... , b_nb)
  ##
  ## If stability.check is TRUE (default) the function will stop if any
  ## poles of A() is outside the unit circle.
  ##
  ## VALUE:  A list with elements:
  ## call:    Image of the call
  ## stable:  Logical indicating if the transfer-function is stable
  ## sg:      Stationary gain (only if stable is true)
  ## n.init:  Number of initial values needed
  ## A:       A
  ## B:       B
  ## delay:   delay

  call <- match.call()
  
  if(A[1] != 1) stop("A[1] must be 1")

  # Stability  
  if(length(A) == 1)
    stable <- TRUE
  else {
    tmp <- A
    tmp[-1] <- - A[-1]  # Def. of signs on params
    stable <- all(abs(polyroot(rev(tmp))) < 1)
  }
  if(stability.check && !stable)
    stop("unstable transfer-function")
  if(!stable && unit.sg)
    stop("unstable system: not possible to make the stationary gain one")
  
  ## Stationary gain
  if(stable) {
    if(length(A) == 1)
      sg <- sum(B)
    else
      sg <- sum(B) / (1 + sum(-A[2:length(A)]))
  }
  else
    sg <- NA
  
  if(unit.sg) {
    B <- B/sg
    sg <- 1
  }

  ## Number of initial values needed
  n.init <- length(A) - 1

  ## Return value
  z <- list(call=call, stable=stable, sg=sg, n.init=n.init,
            A=A, B=B, delay=delay)
  return(structure(z, class="rtf"))
}


rtf.filter <- function(x, rtfobj, init)
{
  ## Filter x using a rational transfer function object (rtfobj) as created by rtf().
  ## If initialization is needed 'init' is supplied to the recursive filter (the first).
  ##
  ## Note that:
  ##
  ## * 'init' is multiplied with the stationary gain of the recursive filter before
  ##   it is applied, i.e. replaced by init/A(1).
  ##
  ## * First the series is filtered trough 1/A(q^-1), and the initialization is in
  ##   terms of the output of this filter.  Furthermore, 'init' is used to calculate
  ##   the first value of the filtered series, i.e. 'init' corresponds to times
  ##   0, -1, -2, ...
  ##
  ## * The causal convolution filter cannot return values for time <= length(B) - 1,
  ##   since it do not use initialization.
  ##
  ## * Since the recursive filter is run first (an no missing i allowed in x)
  ##   the bug in filter() when the series starts with NA will not become active.

  if(!rtfobj$stable)
    warning("unstable filter")
  
  if(any(is.na(x)))
    stop("missing values in x not allowed")
  
  ## z_t = 1 / A(q^-1) x_t 
  if(rtfobj$n.init > 0) {
    if(missing(init))
      init <- x[1:rtfobj$n.init]
    init <- init / (1 + sum(-rtfobj$A[2:length(rtfobj$A)]))
    x <- filter(x=x, filter=rtfobj$A[2:length(rtfobj$A)], method="rec", init=init)
  }
  
  ## B(q^-1) z_t = B(q^-1) / A(q^-1) x_t
  x <- filter(x=x, filter=rtfobj$B, method="con", sides=1) 

  ## Delay (H(q) x_t)
  x <- as.numeric(x)
  x <- c(rep(NA, rtfobj$delay), x[(1+rtfobj$delay):length(x)])
  
  ## Return filtered series
  return(as.numeric(x))
}


rtf.impulse <- function(rtfobj, lag.max, plot.it=TRUE,
                        nzero=2, type="h",
                        xlab="Lag", ylab="Impulse Response", ... )
{
  ## Impulse response of rtfobj (one like the one created by rtf()),
  ## i.e. the response on a unit impule corresponding to index 1 of
  ## the output.
  init <- rep(0, rtfobj$n.init)
  x <- rep(0, length(rtfobj$B) - 1 + 2*rtfobj$delay)
  x <- c(x, 1, rep(0, lag.max))
  y <- rtf.filter(x=x, rtfobj=rtfobj, init=init)
  idx <- length(y) - rtfobj$delay - (lag.max:0)
  y <- y[idx]
  names(y) <- as.character(0:lag.max)
  if(!plot.it)
    return(y)
  plot(c(rep(0,nzero),0:lag.max), c(rep(0,nzero),y),
       type=type, xlab=xlab, ylab=ylab, ...)
  abline(h=0)
  invisible(return(y))
}


rtf.step <- function(rtfobj, lag.max, plot.it=TRUE,
                     nzero=2, type="h",
                     xlab="Lag", ylab="Step Response", ylim, ... )
{
  ## Step response of rtfobj (one like the one created by rtf())
  ## i.e. the response on a unit step corresponding to index 1 of
  ## the output.
  init <- rep(0, rtfobj$n.init)
  x <- rep(0, length(rtfobj$B) - 1 + 2*rtfobj$delay)
  x <- c(x, rep(1, lag.max+1))
  y <- rtf.filter(x=x, rtfobj=rtfobj, init=init)
  idx <- length(y) - rtfobj$delay - (lag.max:0)
  y <- y[idx]
  names(y) <- as.character(0:lag.max)
  if(!plot.it)
    return(y)
  if(missing(ylim))
    ylim <- sort(range(c(0,rtfobj$sg,y)))
  plot(c(rep(0,nzero),0:lag.max), c(rep(0,nzero),y),
       type=type, xlab=xlab, ylab=ylab, ylim=ylim, ...)
  abline(h=0)
  if(rtfobj$stable)
    abline(h=rtfobj$sg, lty=2)
  invisible(return(y))
}


print.rtf <- function(x, digits, ...)
{
  rtfobj <- x
  cat("Rational transfer function (rtf) object.\n")
  cat("Call:\n")
  print(rtfobj$call)
  cat(paste(ifelse(rtfobj$stable, "Stable", "UNSTABLE"),
            "filter with definition (coef. rounded in display):\n"))
  if(missing(digits))
    digits <- options()$digits
  digits <- max(3, digits-3) 
  A <- signif(rtfobj$A, digits=digits)
  B <- signif(rtfobj$B, digits=digits)
  delay <- rtfobj$delay
  if(length(A) == 1)
    A.poly <- paste(A)
  else {
    A.poly <- c(1, paste(A[-1], " q^-", 1:(length(A)-1), sep=""))
    A.poly <- paste(A.poly, collapse=" - ")
  }
  if(length(B) == 1)
    B.poly <- paste(B)
  else {
    B.poly <- c(B[1], paste(B[-1], " q^-", 1:(length(B)-1), sep=""))
    B.poly <- paste(B.poly, collapse=" + ")
  }
  if(delay != 0) 
    delay.txt <- paste("q^-", delay, sep="")
  else
    delay.txt <- ""
  empty.space <- paste(rep(" ", nchar(delay.txt)), collapse="")
  cat("\n")
  cat("\t", empty.space, B.poly, "\n")
  cat("\t", delay.txt,
      paste(rep("-", max(nchar(B.poly), nchar(A.poly))), collapse=""), "\n")
  cat("\t", empty.space, A.poly, "\n")
  cat("\n")  
  cat(paste("Stationary gain:", rtfobj$sg), "\n")
  cat(paste("Number of initial values needed for filtering:", rtfobj$n.init), "\n")
  invisible(return(rtfobj))
}


plot.rtf <- function(x, lag.max, ...)
{
  ## Graphical representation of an object of class 'rtf'.  If lag.max is not specified reasonable things happens.  '...' are passed to rtf.impulse and rtf.step.

  oldpar <- par()
  on.exit(par(oldpar))
  par(mfrow=c(2,2))

  zeros <- polyroot(rev(x$B))
  tmp <- x$A
  tmp[-1] <- - x$A[-1]  
  poles <- polyroot(rev(tmp))  # Bug in calc. of poles for ploting fixed Wed Sep 5 2001

  par(pty="s")

  plot(complex(modulus=1, argument=seq(0,2*pi,length=200)),
       type="l",
       xlab="Re", ylab="Im",
       main="Zeros",
       xlim=c(-1,1)*max(1, abs(zeros)),
       ylim=c(-1,1)*max(1, abs(zeros)))
  abline(h=0,v=0,lty=2)
  if(length(zeros) > 0)
    points(zeros, pch=1)
  else
    text(0, 0, "No Zeros")
  
  plot(complex(modulus=1, argument=seq(0,2*pi,length=200)),
       type="l",
       xlab="Re", ylab="Im",
       main="Poles",
       xlim=c(-1,1)*max(1, abs(poles)),
       ylim=c(-1,1)*max(1, abs(poles)))
  abline(h=0,v=0,lty=2)
  if(length(poles) > 0)
    points(poles, pch=1)
  else
    text(0, 0, "No Poles")

  par(pty="m")

  if(missing(lag.max))
    lag.max <- ceiling(3 / (1 - max(abs(poles)))) + x$delay
  if(is.na(lag.max))
    lag.max <- 1
  lag.max <- lag.max + ceiling(1.5*length(x$B))

  if(x$stable) {
    rtf.impulse(x, lag.max=lag.max, ...)
    rtf.step(x, lag.max=lag.max, ...)
  }
  else
    warning("Unstable filter impulse and step response not plotted")
            
  invisible(return())
  
}
