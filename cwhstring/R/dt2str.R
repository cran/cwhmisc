dt2str <- function(dt,verbose=FALSE) {
  hr <- dt %/% 3600; rest <- dt %% 3600
  mi <- rest %/% 60; rest <- rest %%60
  se <- rest
  if (verbose) paste(hr,"hours",mi,"minutes",se,"seconds",sep=" ")
  else paste(hr,mi,se,sep=":")
}
