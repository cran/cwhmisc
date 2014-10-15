.RC <- function(dir,pkg,sw=c(0,2,5)) {
    pp <- paste(dir,"/",pkg,sep="")
#     if (sw == 1) system(paste("rm ",pkg,".pdf",sep="")) 
    if (sw==2) for (f in list.files(paste(pp,"/R",sep=""), full.names=TRUE)) parse(f) # 2015-06-17
    system(paste("R CMD ",switch (as.character(sw),
   "1" = paste("Rd2pdf", pp," --no-clean --force"),
   "2" = "check",
   "3" ="build --force",
   "4" = paste("check --as-cran ",getwd(),pkg,".tar.gz"),
   "5" = "check --as-cran",
   "6" = "install"
      )  ," ",dir,"/",pkg,sep=""))
}

RCA <- function(dir, pkg, sw=0:6, echoonly=FALSE) {
S <-c(
  " 1  =  Rd2pdf --no-clean --force", ## look at mypkg.Rcheck/mypkg-manual.pdffor problems
  " 2  = check",
  " 3  = build --force",
  " 4  = check --as-cran <pkg>.tar.gz",
  " 5  = check --as-cran",
  " 6  = install"
      )
                          
  if (0 %in% sw) {
    for (ii in seq_along(S)) print(S[ii])
  }

  for (kk in setdiff(sw,0)) {
      print(paste(" ==== RC  ", S[kk], paste(dir,pkg,sep=" /")," on: ",datetime()))
      if (!echoonly) .RC(dir,pkg,kk)
  }
}  ##  RCA

RR <- function(sw=0:6,echoonly=FALSE) RCA("/Users/hoffmann/R","cwhmisc",sw=sw,echoonly=echoonly)
