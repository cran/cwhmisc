\name{like}
\alias{like}
\title{Prepare new data for prediction}
\description{
  Prepare data to conform to structure of data used in a model, for use as new data
}
\usage{
 like(X, ...)
}
\arguments{
  \item{X} {Data used in linear model like \code{lm}}
  \item{...} {List of other arguments to model to be used for prediction}
}
\value{
	Data usable for  \code{predict} which are now compatible, all factors have the same level set, and so forth.
}
\examples{
\dontrun{
 foo <- lm(formula, data=foe)
 fie <- like(foe, sex="F", age=20:25, smoke="1-7")
 predict(foo, newdata=fie)
 }
}
\author{Peter Dalgaard, \email{p.dalgaard@biostat.ku.dk}
}
\keyword{models}

