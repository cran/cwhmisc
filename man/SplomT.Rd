\name{SplomT}
\alias{SplomT}
\title{splom with title and time stamp}
\description{
 SplomT creates a scatterplot matrix with a: covariances (with
script size proportional to size) in the upper triangle, b: histograms
(with smoothing) and variable names in the diagonal, and c: scatterplot
with smoothes in y and x direction in the lower triangle, stressing high
correlations by nearly parallel lines. See figure in other documentation.
}
\usage{
  SplomT(data,
        mainL = deparse(substitute(data)), xlabL = "", 
        hist = "h", adjust = 1,
        hist.col = trellis.par.get("strip.background")$col[5],
        cex.diag = 1,
        h.diag=0.4,
        colYonX = "red",
        colXonY = "blue", ...)
}
\arguments{
  \item{hist.col}{string, color of the histogram; like "(hash)ffccff" }
  \item{data}{Matrix or dataframe containing data, variables in columns}
  \item{mainL}{Label on top of scatterplot matrix or matrix of histograms}
  \item{xlabL}{Label for x-axis}
  \item{hist}{"h" = histogram, "d" = density curve, "b" = both}
  \item{adjust}{factor to adjust smoothing window for density curve}
  \item{cex.diag}{correction factor for font height of correlations and
	names in the diagonal}
  \item{h.diag}{placement of the variable name in the diagonal panel, =0
	means on the lower border, = 0.5 in the middle between lower and
	upper border}
  \item{colYonX, colXonY}{colour of smoothing lines, y on x and x on y}
  \item{\ldots}{Parameters passed on to upper.panel,lower.panel,diag.panel}
}
\value{
	This function is called for its side effect to produce a plot.
}
\examples{
  nc <- 8 # number of columns 
  nr <- 250 # number of rows
  data <- as.data.frame(matrix(rnorm(nr*nc),nrow=nr,ncol=nc))
  data[,nc]   <- data[,nc-2] + 0.3*data[,nc-1] #generate higher correlations
  data[,nc-1] <- data[,nc-1] + 0.9*data[,nc]
  colnames(data)<-paste("vw",letters[1:nc],sep="")
  SplomT(data,mainL="",hist="d",cex.diag=0.6,hist.col="green")
  SplomT(data,mainL="",hist="b",adjust=0.4,cex.diag = 0.5)
}
\author{
  Christian W. Hoffmann, \email{christian@echoffmann.ch}, with the assistance of
  Deepayan Sarkar \email{Deepayan.Sarkar@r-project.org}.
}
\keyword{hplot}


