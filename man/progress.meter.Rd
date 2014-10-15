\name{progress.meter}
\alias{progress.meter}
\title{Monitor the progress of a repetitive calculation.}
\description{
  \code{progress.meter} writes a symbol to the output at each
	invocation. The symbol is usually a ".", a "+" if i \%\% == 0,
	and (i \%/\% 10) \%\% 10 if i \%\% 10 == 0. If i \%\% 50 == 0, a
	line break will be written and i printed.
}
\usage{
  progress.meter(i) # inside a function or loop
}
\arguments{
  \item{i}{Integer.}
}
\value{
	invisible(NULL).
}
\examples{
  n <- 1 # adjust
  for (i in 0:250) {
    kk <- 0
    for (mm in 1:10^n) {
      kk <- kk+1  # do something time consuming
    }
    progress.meter(i)
  }
  cat("")
#    0....+....1....+....2....+....3....+....4....+....
#   50....+....6....+....7....+....8....+....9....+....
}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{print}
