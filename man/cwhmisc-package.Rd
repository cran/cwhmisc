\name{cwhmisc}
\alias{cwhmisc-package}
\alias{cwhmisc}
\docType{package}
\title{A collection of useful functions and constants}
\description{
Useful functions and constants for mathematics, astronomy, plotting, printing, data manipulation, statistics, string manipulation, etc.
}
\details{
\tabular{ll}{
Package: \tab cwhmisc\cr
Type: \tab Package\cr
Version: \tab 6.0\cr
Date: \tab 2015-07-30\cr
License: \tab GPL (>= 2)\cr
Author: \tab  Christian W. Hoffmann <christian@echoffmann.ch> www.echoffmann.ch\cr
Maintainer: \tab same
}
}
\author{
Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{ package }
\examples{
\dontrun{ #  Show use of 'SplomT'
  nr <- 100; nc <- 8;
  data <- as.data.frame(matrix(rnorm(nr*nc),nrow=nr,ncol=nc))
  data[,nc]   <- data[,nc-2] + 0.3*data[,nc-1] #generate higher correlations
  data[,nc-1] <- data[,nc-1] + 0.9*data[,nc]
  colnames(data)<-paste("vw",letters[1:nc],sep="")
#  splom(~data,cex=0.2)
  try( SplomT(data,mainL="SplomT with random data",hist="d",cex.diag=0.6,hist.col="green") )
}
}
