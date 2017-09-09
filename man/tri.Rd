\name{triplot}
\alias{triplot}
\title{Ternary or Triangular Plots.}
\description{
  \code{plotCI} plots in a triangle the values of three variables. Useful for mixtures 
	(chemistry etc.).
}
\usage{
triplot(a, f, m, symb=2, grid=FALSE, ...)
}
\arguments{
  \item{a}{Vector of first variable.}
  \item{f}{Vector of second variable.}
  \item{m}{Vector of third variable.}
  \item{symb}{Symbol to be plotted}
  \item{grid}{Plot the grid: TRUE or FALSE}
  \item{\dots}{Additional parameters for \code{plot}}
}
\value{
  The function \code{tri} is called for its side effect to produce a plot.
}
\author{
  Colin Farrow
Computing Service, University of Glasgow, Glasgow G12 8QQ\cr
, \email{c.farrow@compserv.gla.ac.uk}
}
\examples{
# some random data in three variables
c1 <- runif(25)
c2 <- runif(25)
c3 <- runif(25)
# basic plot
par(mfrow=c(1,2))
triplot(c1,c2,c3)
# plot with different symbols and a grid
triplot(c1,c2,c3, symb=7, grid=TRUE)
}
\keyword{hplot}
