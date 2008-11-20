lowess.bygroup <- function(x, y, group, lin=FALSE, col = par("col"), bg = NA, pch = par("pch"), cex = 1, ...) {
  for (ii in unique(group)) {
    ind <- complete.cases(x[ii == group],y[ii == group])
    cc  <- cbind(x[ii == group][ind],y[ii == group][ind])
    oi  <- order(cc[,1])
    cat(paste(ii,length(oi),"\n"))
    if (lin) lines(x[oi],y[oi],lty=2)
    if (length(oi)>0) lines(lowess(x[oi],y[oi]))
  }
  invisible()
} 

loess.bygroup <- function(x, y, group, lin=FALSE, col = par("col"), bg = NA, pch = par("pch"), cex = 1, ...) {
  for (ii in unique(group)) {
    ind <- complete.cases(x[ii == group],
                          y[ii == group])
    cc  <- cbind(x[ii == group][ind],y[ii == group][ind])
    oi  <- order(cc[,1])
    cat(paste(ii,length(oi),"\n"))
    if (lin) lines(x[oi],y[oi],lty=2)
    if (length(oi)>0) lines(x[oi],predict(loess(y[oi]~x[oi]),x[oi]))
  }
  invisible()
}

