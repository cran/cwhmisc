str2formula <- function(s) {
  formula(paste(s[[1]],s[[2]],sep="~"))
}

term.names2formula <- function(ls,rs) {
  formula(paste(ls,paste(rs,collapse="+"),sep="~"))
}
