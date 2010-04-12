\name{interpol}
\alias{setupInterp}
\alias{evalInterp}
\alias{minInterp}
\alias{quadmin}
\title{Polynomial and rational interpolation
}
\description{
Determine the argument of the minimum by polynomial or rational interpolation of given points
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setupInterp(x, y, doPoly = TRUE)
evalInterp(xi, ss)
minInterp(x, y, add = FALSE, doPoly = TRUE)
quadmin(x, y)
}
\arguments{
  \item{x}{vector of x-coordinates}
  \item{y}{vector of y-coordinates}
  \item{xi}{argument x of interpolation}
  \item{ss}{setup given by setupInterp}
  \item{add}{if TRUE one more point is used than for FALSE (default)}
  \item{doPoly}{if TRUE polynomial interpolation is used, if FALSE rational interpolation is used, with three points and four points respectively (for add=FALSE}
}
\value{
  \item{xmin }{x-value of the minimum. NA if too few points are given or no minimumm exists in }
}
\references{
Stoer, J., 1989. Numerische Mathematik 1ed. 5. Springer, Berlin.  Applied and Computational Complex Analysis, Vol.2. Wiley, 
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\note{

There is a test program "InterpPR.pdf" available
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x <- c(1,2,4,6)
minInterp(x,(x-3)^2,add=FALSE,doPoly=TRUE)
minInterp(x,(x+1.0/x),add=FALSE,doPoly=FALSE)
minInterp(x,(x+1.0/x),add=TRUE,doPoly=TRUE)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( x, f, add = FALSE, PolyInt=TRUE ) {
  # Interpolate (PolyInt=TRUE) three points (Newton)
  # or four points (x,f) (Thiele),
  # For add=TRUE one more point is used.
  # Returns the numerical minimum and the explicit interpolant,
  #   or NA if too few points are given.
  ks <- sort.list(x)
  x  <- x[ks];  f <- f[ks] 
  nn <- length(x)
  km <- which.min(f)
  bm <- 4+add-PolyInt
  if (nn < bm) {
    if (add && nn == bm-1) { # add == FALSE would also work
      add <- FALSE
      bm  <- bm-1
    } else {
      return( list(xmin=NA, int=list(fct=NA, coef=NA, xx=NA)))
    }
  }
  if (nn >= bm) {
    k <- max(1,min(km-round(bm/2),nn-bm+1))
    x <- x[k:(k+bm-1)];  f <- f[k:(k+bm-1)]        
  }
  s <- SetupIntp( x, f, PolyInt )
  if ( PolyInt )  { #  Newton
    if (add==0)  s$q[4] <- 0.0  # not needed: s$x[3] <- 0.0
    a <- s$x[1] + s$x[2]
    b <- a + s$x[3]
    c <- (s$x[2] + s$x[3])*s$x[1] + s$x[2]*s$x[3]
            # coefficients for quadratic equation for minimum   
    A <- 3*s$q[4]                      # coeff of x^2 
    B <- (s$q[3] - s$q[4]*b)*2         # coeff of x
    C <- s$q[2] - a*s$q[3] + c*s$q[4]  # absolute term
    D <- ((-s$q[4]*s$x[3] + s$q[3])*s$x[2] - s$q[2])*s$x[1] + s$q[1]  
    coef <- c(A/3, B/2, C, D) # coefficients for interpolant
    fct  <- function(co,x) (co[1]*x^3 + co[2]*x^2 + co[3]*x + co[4])
  } else { #  Thiele
    z0 <- s$q[bm];  z1 <- z2 <- n1 <- n2 <- 0;    n0 <- 1;
    for (ii in ((bm-1):1)) {
      sq <- s$q[ii];  sx <- s$x[ii]
      if (abs(z0) < cMaxRealBy38) {
        Z0 <- sq*z0 - sx*n0
        Z1 <- sq*z1 - sx*n1 + n0
        Z2 <- sq*z2 - sx*n2 + n1
        # not needed    z3 <- n2  
        n0  <- z0;  n1 <- z1;  n2 <- z2
        z0  <- Z0;  z1 <- Z1;  z2 <- Z2
      } else {  # "restart"
        z0 <- sq;  z1 <- z2 <- n1 <- n2 <- 0;    n0 <- 1;
      }
    }
 # coefficients for quadratic equation for minimum   
    A <- z2*n1 - z1*n2      # coeff of x^2
    B <- 2*(z2*n0 - z0*n2)  # coeff of x
    C <- z1*n0 - z0*n1      # absolute term
    coef <- c(z2,z1,z0,n2,n1,n0)
    cc   <- coef[1]
    if (abs(cc) > 1000.0) coef <- coef/cc
    fct <- function(co,x)(co[1]*x^2+co[2]*x+co[3])/(co[4]*x^2+co[5]*x+co[6])
  }
  
  xmin <- sort(solveQeq( A, B, C ), na.last=TRUE)
  if (!inrange(Re(xmin[1]),x)) {
    xa <- xmin[1];  xmin[1] <- xmin[2];  xmin[2] <- xa 
  }
  if (!PolyInt && abs(n2)/16.0+1.0 == 1.0) { # high chance of unreachable points
    if ( abs(n1)/16+1.0 == 1.0) { # constant denominator
      xmin <- sort(solveQeq( z2, z1, z0 ), na.last=TRUE)
      coef <- c(z2,z1,z0)
      fct <- function(co,x)(co[1]*x^2+co[2]*x+co[3])
      
    } else {
      xn <- sort(solveQeq( 0.0, n1, n0 ), na.last=TRUE)[1]
      if (any(abs(xmin-xn)/16+1.0 == 1.0)) { # common zeros
        xmin <- NA           # of numerator and denominator
        coef <- c(z2/n1,z0/n0)
        fct <- function(co,x)(co[1]*x+co[2])
      }
    }
  }
  return( list(xmin=xmin, int=list(fct=fct, coef=coef, xx=s$x)) )
  }  ## minIntp
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
\keyword{ symbolmath }% __ONLY ONE__ keyword per line
