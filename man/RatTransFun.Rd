\name{rtf}
\alias{rtf}
\alias{rtf.filter}
\alias{rtf.impulse}
\alias{rtf.step}
\alias{printrtf}
\alias{plotrtf}
\title{Rational Transfer Function objects for R}
\description{ no detqils given.
}

\usage{
rtf(A = 1, B = 1, delay = 0, unit.sg = TRUE, stability.check = TRUE)
rtf.filter(x, rtfobj, init)
rtf.impulse(rtfobj, lag.max, plot.it=TRUE,
                        nzero=2, type="h",
                        xlab="Lag", ylab="Impulse Response",\dots)
rtf.step(rtfobj, lag.max, plot.it=TRUE,
                     nzero=2, type="h",
                     xlab="Lag", ylab="Step Response", ylim,\dots)
printrtf(x, digits,\dots)
plotrtf(x, lag.max,\dots)
}

\arguments{
  \item{A}{See details}
  \item{B}{See details}
  \item{delay}{delay}
  \item{unit.sg}{See details}
  \item{stability.check}{See details}
  \item{x}{Filter (print.rtf), or (plot.rtf) rational transfer function object as created by rtf()}
  \item{rtfobj}{rational transfer function object as created by rtf()}
  \item{init}{If initialization is needed 'init' is supplied to the recursive filter (the first)}
  \item{lag.max}{maximum lag}
  \item{plot.it}{if TRUE generate the plot}
  \item{nzero}{number of zeros}
  \item{type}{see \code{\link{plot.default}}}
  \item{xlab}{label at x-axis}
  \item{ylab}{label at y-axis}
  \item{ylim}{limit in y-direction}
  \item{digits}{for print}
  \item{...}{passed to rtf.impulse and rtf.step}
}
\details{
rtf:	
  Creates and checks a rational transfer-function object
   
  $y_t = H(q) x_t$, where $H(q) = q^-delay B(q^{-1}) / A(q^{-1})$
   
  If unit.sq is TRUE (default) the coefs. of $B(q^{-1})$ is multiplied
  with a factor making the stationary gain one.
   
  $A(q^{-1}) = 1   - a_1 q^{-1} - ... - a_{na} q^{-na}$
  $B(q^{-1}) = b_0 + b_1 q^{-1} + ... + b_{nb} q^{-nb}$
   
  Note that '-' is used in A() and '+' in B(), these are specified as:
   
  $A = c(1,   a_1, .... , a_{na})$
  $b = c(b_0, b_1, .... , b_{nb})$
   
  If stability.check is TRUE (default) the function will stop if any
  poles of A() is outside the unit circle.

rtf.filter:
  Filter x using a rational transfer function object (rtfobj) as created by rtf().
  If initialization is needed 'init' is supplied to the recursive filter (the first).
   
  Note that:
   
  * 'init' is multiplied with the stationary gain of the recursive filter before
    it is applied, i.e. replaced by init/A(1).
   
  * First the series is filtered trough $1/A(q^{-1})$, and the initialization is in
    terms of the output of this filter.  Furthermore, 'init' is used to calculate
    the first value of the filtered series, i.e. 'init' corresponds to times
    0, -1, -2, ...
   
  * The causal convolution filter cannot return values for time <= length(B) - 1,
    since it do not use initialization.
   
  * Since the recursive filter is run first (an no missing i allowed in x)
    the bug in filter() when the series starts with NA will not become active.

rtf.impulse:
  Impulse response of rtfobj (one like the one created by rtf()),
  i.e. the response on a unit impule corresponding to index 1 of
  the output.

rtf.step:
  Step response of rtfobj (one like the one created by rtf())
  i.e. the response on a unit step corresponding to index 1 of
  the output.
}
\value{
rtf:
  \item{call}{Image of the call}
  \item{stable}{Logical indicating if the transfer-function is stable}
  \item{sg}{Stationary gain (only if stable is TRUE)}
  \item{n.init}{Number of initial values needed}
  \item{A}{A}
  \item{B}{B}
  \item{delay}{delay}
}
\author{Henrik Aalborg Nielsen, IMM, DTU, \email{han@imm.dtu.dk}
adapted by   Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{math}
\keyword{array}
