### R code from vignette source 'cwhmisc.Rnw'

###################################################
### code chunk number 1: Scw
###################################################
#  Show use of 'SplomT'
dontrun <- FALSE # TRUE
if (!dontrun) {
  library(cwhmisc)
  nr <- 100; nc <- 8;
  data <- as.data.frame(matrix(rnorm(nr*nc),nrow=nr,ncol=nc))
  data[,nc]   <- data[,nc-2] + 0.3*data[,nc-1] #generate higher correlations
  data[,nc-1] <- data[,nc-1] + 0.9*data[,nc]
  colnames(data)<-paste("vw",letters[1:nc],sep="")
#  splom(~data,cex=0.2)
  SplomT(data,mainL="SplomT with random data",hist="d",cex.diag=0.6,hist.col="green")
}


