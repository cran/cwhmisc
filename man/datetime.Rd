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
datetime()  #[1] "2001-02-07, 15:24:51"
mydate()    #[1] "2001-02-07"
mytime()    #[1] "15:24:12"
}
\note{These functions are implemented using POSIX}
\author{
  Christian W. Hoffmann <c-w.hoffmann@sunrise.ch> \cr
  \url{http://www.wsl.ch/personal_homepages/hoffmann/index_EN}
}
\keyword{chron}
\keyword{print}
