my.var <- function(x, y=NULL) {  ## omit rows with NA
#   if (!missing(y)) x <- cbind(x,y)
#   res <- var(x[rowSums(is.na(x)) == 0,])
#   if (dim(res)[1]==2) res <- res[1,2]
#   else  dimnames(res) <- list(names(x)[1],names(x)[1])
#   res
  if (!missing(y)) x <- cbind(x,y)
  var(x, y, na.rm = TRUE, use = "complete")
}
my.cor <- function(x, y=NULL) {  ## omit rows with NA
#   if (!missing(y)) x <- cbind(x,y)
#   res <- cor(x[rowSums(is.na(x)) == 0,])
#   if (dim(res)[1]==2) res <- res[1,2]
#   else  dimnames(res) <- list(dimnames(x)[[2]],dimnames(x)[[2]])
#   res
  if (!missing(y)) x <- cbind(x,y)
  cor(x, y, use = "complete")
}
