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
datetime()  #[1] "2014-10-03, 16:00:03"
mydate()  #[1] "2014-10-03"
mytime()  #[1] "16:00:09"
}
\note{These functions are implemented using POSIX}
\author{
  Christian W. Hoffmann <christian@echoffmann.ch>
}
\keyword{chron}
\keyword{print}
