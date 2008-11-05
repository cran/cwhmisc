\name{scode}
\alias{scode}
\title{Generate the significance codes as in summary.lm}
\description{Generate the significance codes as in summary.lm
}
\usage{
  scode(p)
}
\arguments{
  \item{p}{Probability}
}
\value{
	Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
}
\examples{
  for (ii in c(0.005, 0.02,0.0501,0.2)) { scode(ii) }
}
\note{
  lifted from stats::printCoefmat
}
\author{
  Christian W. Hoffmann, \email{christian.hoffmann@wsl.ch},
  \url{http://www.wsl.ch/staff/christian.hoffmann}
}
\keyword{regression}
