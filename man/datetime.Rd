\name{datetime}
\alias{mydate}
\alias{mytime}
\alias{datetime}
\title{Show date and time in ISO format}
\description{
   \code{datetime()} outputs date and time in ISO format
}
\usage{
datetime(); mydate(); mytime()
}
\arguments{
none
}
\value{
character string 
}
\examples{
datetime()  #[1] "2012-09-27, 13:56:38"
mydate()  #[1] "2012-09-27"
mytime()  #[1] "13:56:39"
}
\note{These functions are implemented using POSIX}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch>
}
\keyword{chron}
\keyword{print}
