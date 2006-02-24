plt <- function(VIEW,f,file="",horizontal = FALSE) { ## C.Hoffmann,  2003-09-16
 ## f MUST NOT contain a call to postscript !!
  if (is.na(VIEW)) VIEW <- "NA"
  if (file=="") file <- "Rplots"
  switch (VIEW,
    "NA"   = NA,
    "see"  = f,
    "eps"  = {f; dev.copy2eps(file=paste(file,".eps",sep=""),horizontal = horizontal, onefile = FALSE, paper = "default")},
    "ps"   =,"ps+p" =
             {postscript(file=paste(file,".ps",sep=""),print.it=VIEW=="ps+p",onefile=FALSE, horizontal = horizontal, paper = "default");
              on.exit({dev.off();dev.set(dev.prev())}); f},
    "pdf"  = {pdf(file=paste(file,".pdf",sep=""));
              on.exit(dev.off()); f},
    print(">>> plt:  Use VIEW=c('NA','see','eps','ps','ps+p','pdf')")
  )
  invisible()
} ## end plt

plotTitStamp <- function (rows, cols, tit="", stampl="", f = function(x) 0, cex = 1.5, 
    reset = TRUE, outer = TRUE, oma = c(2, 2, 4, 2), mar = c(4, 4, 2, 1)) 
{
    oldpar <- par(mfrow = c(rows, cols), oma = oma, mar = mar)
    if (reset) 
        on.exit(par(oldpar))
    f
    L <- 1
    if (length(tit) > 1) { L <- 2; mtext(tit[1], side = 3, line = 2, outer = outer, cex = cex, adj = 0.5);}
    mtext(tit[L], side = 3, line = 2-L, outer = outer, cex = cex, 
        adj = 0.5)
    mtext(paste(stampl, if (nchar(stampl)) 
        ", ", datetime()), side = 1, line = 0, outer = outer, 
        cex = 0.5, adj = 1)
}  # end pltTitStamp

pltTSV <- function(VIEW="see",rows, cols, tit, stampl, f=function(x) 0, cex=1.5,reset=TRUE,outer=TRUE,oma=c(2,2,4,2),mar=c(4,4,2,1),file=stampl,horizontal=TRUE){ ## f MUST NOT contain a call to postscript !!
  plt(VIEW,plotTitStamp(rows, cols, tit, stampl, f=f, cex=cex,reset=reset,outer=outer,oma=oma,mar=mar),file=file,horizontal=horizontal)
} ## end pltTSV

pltCharMat <- function(m,tit) {
  n22 <- n2cCompact(m)
  cM <- charMat(n22)
  xl <- lattice:::extend.limits(range(cM$x),prop=0.07)
  plot(xl,range(cM$y),type="n",main=paste(tit,", ",rev(n22)[1],sep=""))
  text(cM$x,cM$y,cM$tx)
}  ## pltCharMat

