RC <- function(dir,pkg,type="c") {system(paste("R CMD ",switch(type,
  "a"="check --as-cran",
  "b"="build --force",
  "c"="check",
  "i"= "install")," ",dir,"/",pkg,sep=""))}
RCA <- function(dir,pkg,trig=c(0,2:5)) {
  if (0 %in% trig) print(" For checking as required by CRAN policy, run RC() with pkg='pkg_VERSION.tar.gz'")
  if (1 %in% trig) {print(paste("== RC - c",dir,pkg)); RC(dir,pkg,"c")}
  if (2 %in% trig) {print(paste("== RC - b",dir,pkg)); RC(dir,pkg,"b")}
  if (3 %in% trig) {print(paste("== RC - c",dir,pkg)); RC(dir,pkg,"c")}
  if (4 %in% trig) {print(paste("== RC - a",dir,pkg)); RC(dir,pkg,"a")}
  if (5 %in% trig) {print(paste("== RC - i",dir,pkg)); RC(dir,pkg,"i")}
}
