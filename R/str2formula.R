str2formula <- function(s) {
  formula(paste(s[[1]],s[[2]],sep="~"))
}

term.names2formula <- function(ls,rs) {
  formula(paste(ls,paste(rs,collapse="+"),sep="~"))
}

formula2string <- function(form)  {
  ## take a formula and return the left and the right hand sides
  dput(form,"xxxxformula")
  vec <- scan(file="xxxxformula",character(),quiet = TRUE)
  str <- paste(vec, sep=" ", collapse="")
  tilde <- substring.location(str,"~")$first[1]
  unlink("xxxxformula")
  list(left = substring(str,1,tilde-1), right = substring(str,tilde+1))
# list(left = if (attr(terms(form),"response")==0) "" else as.character(attr(terms(form),"variables"))[2], right = attr(terms.formula(form),"term.labels"))
}

formula2term.names <- function(form,side)  {
  ##  take a formula and return the names of the terms of the side hand side
  fstr <- formula2string(form)
  strsplit(unlist(fstr[names(fstr)==side], use.names=FALSE),"\\+")
}

formula2Rterm.names <- function(form)  {
  formula2term.names(form,"right")
}
