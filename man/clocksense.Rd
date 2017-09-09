\name{clocksense}
\alias{IsCounterCl2}
\alias{IsCounterCl3}
\alias{CounterClock}
\alias{NoneClock}
\alias{Clockwise}
\alias{ClockSense2}
\alias{ClockSense3}
\title{Functions for directed arcs}
\description{
 Functions for clocksense, i.e. directed arcs
}
\usage{
IsCounterCl2( U, V, ref )
IsCounterCl3( U, V, W, ref )
ClockSense2( U, V, ref )
ClockSense3( U, V, W, ref )
}
\arguments{
  \item{ref,U,V,W}{Real}
}
\details{
\code{CounterClock, NoneClock, Clockwise} = "clckws", "Cntclck","noneclck", "clckws"\cr
\code{ClockSense2} Return the clock sense of U and V\cr
\code{ClockSense3} Return the clock sense of U, V, W\cr
\code{IsCounterCl2} Check if the directed angle from U towards W is counter clockwise, including U==W. Ref is the measure of a full circle, 360 for degrees, 2*Pi for radians, 400 for gon\cr
\code{IsCounterCl3} Check if U, V, W form a counterclock wise sequence.
}
\examples{
ClockSense2(0,220,360) # "clckws"
ClockSense2(0,170,360) # "Cntclck"
ClockSense2(0,0,360) # "noneclck"
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{character,arith}
