\name{functions}
\alias{inrange}
\alias{modulo}
\alias{moduloSymm}
\alias{zero}
\alias{one}
\alias{onebyx}
\alias{sqr}
\alias{powr}
\alias{equal}
\alias{equalFuzzy}
\alias{quotmean}
\alias{safeDiv}
\alias{solveQeq}
\alias{IsCounterCl2}
\alias{IsCounterCl3}
\alias{CounterClock}
\alias{NoneClock}
\alias{Clockwise}
\alias{ClockSense2}
\alias{ClockSense3}
\title{Functions for testing and other}
\description{
 Functions for testing on equality, exactly or with a tolerance, functions usable as parameters in other functions.
}
\usage{
modulo( m, n  )
moduloSymm( m, n )
one ( x )
onebyx( x )
sqr( x )
powr( a, x )
equal( x, y )
equalFuzzy( x, y, prec, rel=TRUE )
quotmean(x,y)
safeDiv( num, den )
solveQeq(a,b,c)
inrange(x,r)
IsCounterCl2( U, V, ref )
IsCounterCl3( U, V, W, ref )
ClockSense2( U, V, ref )
ClockSense3( U, V, W, ref )
}
\arguments{
  \item{m,n,num,den}{Integer}
  \item{a,b,c,prec,r,ref,x,y,U,V,W}{Real}
  \item{rel}{Boolean}
}
\details{
\code{modulo} m%%n\cr
\code{moduloSymm} (m - (trunc( m/n )*n)), symmetric to 0.\cr
\code{zero} Return 0.0\cr
\code{one} Return 1.0\cr
\code{onebyx} 1.0/x\cr
\code{sqr} x^2\cr
\code{powr} a^x, with  0^0 := 1, 0^x := 0\cr
\code{equal} x == y\cr
\code{equalFuzzy} One can choose between relative and absolute precision\cr
\code{safeDiv} Compute quotient, safeguard >= cMAXREALBY3Q\cr
\code{quotmean} Compute mean of non-NA elementsof x and y\cr
\code{solveQeq} Solve the quadratic equation given by the coefficients, return two solutions if a != 0, else one solution, possibly NA\cr
\code{inrange} Check if x is in the range r\cr
\code{CounterClock, NoneClock, Clockwise} "clckws", "Cntclck","noneclck", "clckws"\cr
\code{ClockSense2} Return the clock sense of U and V\cr
\code{ClockSense3} Return the clock sense of U, V, W\cr
\code{IsCounterCl2} Check if the directed angle from U towards W is counter clockwise, including U==W. Ref is the measure of a full circle, 360 for degrees, 2*Pi for radians, 400 for gon\cr
\code{IsCounterCl3} Check if U, V, W form a counterclock wise sequence. 
}
\examples{
modulo((-3:5),4 ) # 1 2 3 0 1 2 3 0 1
moduloSymm((-3:5),4) # -3 -2 -1  0  1  2  3  0  1
x <- 200; y <- x + 0.1
equalFuzzy(x,y,0.1*c(10^(-3:0))) # FALSE  TRUE  TRUE  TRUE
equalFuzzy(x,y,0.1*c(10^(-3:0)),FALSE) # FALSE FALSE FALSE  TRUE
safeDiv(1,0) # 1.552518e+231
solveQeq(0,0,1) # NA NA
solveQeq(0,1,0) # 0
solveQeq(0,1,1) # -1
solveQeq(1,0,0) #  0 0
solveQeq(1,0,1) # 0-1i 0+1i
solveQeq(1,1,0) #  -1  0
solveQeq(1,1,1) #  -0.5-0.866025i  -0.5+0.866025i
solveQeq(sample(1:4,1),sample(1:4,1),sample(1:4,1))
ClockSense2(0,220,360) # "clckws"
ClockSense2(0,170,360) # "Cntclck"
ClockSense2(0,0,360) # "noneclck"
}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{character,arith}