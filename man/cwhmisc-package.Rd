\name{cwhmisc-package}
\alias{cwhmisc-package}
\alias{cwhmisc}
\docType{package}
\title{cwhmisc
}
\description{Miscellaneous Functions for Math, Plotting, Printing,
        Statistics, Strings, and Tools
}
\details{Useful functions and constants for mathematics, astronomy, plotting, printing, data manipulation, statistics, string manipulation, etc.
}
\author{
Christian W. Hoffmann
Maintainer: Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{ package }
\keyword{ math }
\keyword{ character }
\keyword{ data }
\keyword{ manip }
\keyword{ print }
\keyword{ utilities }
\keyword{ hplot }
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
