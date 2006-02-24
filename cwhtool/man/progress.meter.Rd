\name{progress.meter}
\alias{progress.meter}
\title{Monitor the progress of a repetitive calculation.}
\description{
  \code{progress.meter} writes a symbol to the output at each
	invocation. The symbol is usually a ".", a "+" if i \%\% == 0,
	and (ii \%\/\% 10) \%\% 10 if ii \%\% 10 == 0. If ii \%\% 50 == 0, a
	line break will be written and i printed.
}
\usage{
progress.meter(i)
}
\arguments{
  \item{i}{An integer.}
}
\value{
	invisible(NULL).
}
\examples{
cat("\n    0")
for (ii in seq(500)) {
  # do something time consuming
  progress.meter(ii)
}
cat("\n")
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{print}
