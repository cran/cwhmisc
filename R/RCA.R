RCA <- function(dir=getwd(), pkg, Rsty, sw=c(2, 5:7), echoonly=FALSE,
                verbose=TRUE) {
#  datetime <- function( ) format(Sys.time(), "%Y-%m-%d, %X") # gives error??  datetime <- function( ) Sys.time()
  stopifnot(sw %in% 0:7)
  if (missing(pkg)) {
       # need to separate dir and pkg
    pkg <- basename(dir) # where one is
    dir <- dirname (dir) # one up
  }
  if (0 %in% sw) catE("dir")
  pp <- paste(dir, pkg, sep = "/")
  wdold <- if (file.exists(dir)) dir else getwd()
  if (verbose) cat("file.exists(Rsty)")
  stopifnot(file.exists(Rsty))
  oold <- options(warn = -1)
  x <- system2("grep", c("-inr", "version", paste(pp,
     "/DESCRIPTION", sep = "")),, TRUE)
  version <- strsplit(x, ": ")[[1]][2]
  if(verbose)  catE("version")
  S <- paste("R CMD", c(
    " Rd2pdf --no-clean --force",
     ## look at mypkg.Rcheck/mypkg-manual.pdf for problems
    " build --force --no-build-vignettes",
    " check",
    " check --as-cran",
    paste(" check --as-cran ", pp, "_", version, ".tar.gz", sep = ""),
    " install",
    paste0(" Sweave --pdf ", pp, "/vignettes/", pkg, ".Rnw")),
          c(rep(pp,  4), "", pp, ""))
#  catE("S");stop("not-halt")
  if(verbose) catn("Package = ", paste(S[sw], "\n"))
  prinE("\t RCA ( sw==0 ): Show the following alternatives for 'sw':\n")
  if( (0 == sw)[1]) for (ii in 1:7) prinE(paste("\t RCA (", ii,  "): ",
                  S[ii],"\n"))
    if(verbose) cat(" --- old dir, current dir: ", wdold, ", ",
                  getwd(), "\n")
  for(kk in setdiff(sw, 0)) {
    if(echoonly) {
      catn(kk, " ", S[kk])
    } else {
      if(verbose) print(paste("\n", " ==== RCA (", kk, ") : ",
              S[kk], " on: ", datetime()))
      system(S[kk])
    }
  }
#    if(verbose) print(datetime())
    setwd(wdold)
    options(warn = oold$warn)
  return(invisible())
} # RCA
