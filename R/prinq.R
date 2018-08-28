catn <- function(...) {
    cat(...); cat("\n")
}

catE <- function(...) {
  args <- unlist(list(...))
  komma <- FALSE
  quot <- "" # left in for historical reasons
  for (kk in seq_along(args)) {
    if ( komma ) cat(", ")
    vkk   <- args[[kk]]
    iscom <- substring( vkk, 1, 1 ) == "\t"
    if ( iscom ) {
      komma <- FALSE
      vkk <- substring(vkk, 2)
      cat( paste0(quot, vkk, quot, "  ") )
    } else {
      komma <- TRUE
      cat(args[[kk]], " = ")
      ss <- eval.parent(parse(text = vkk))
      if (is.character(ss))
        cat( paste0('"', ss, '"' ) )  else cat( ss )
    }
  } # for
  catn()
  invisible()
} # catE

prinE <- function(..., digits = 4) {
  args <- list(...)
  quot <- "" # left in for historical reasons
  for (kk in seq_along(args)) {
    vkk   <- args[[kk]]
    iscom <- substring( vkk, 1, 1 ) == "\t"
    if ( iscom ) {
      vkk <- substring( vkk, 2 )
      cat( paste0(quot, vkk, quot, "  ") )
    } else {
      cat(args[[kk]], "=\n")
      ss <- eval.parent(parse(text = vkk))
      if (is.character(ss)) {
          cat( paste0('"', ss, '"' ) )
        } else {
          print(eval.parent(parse(text = args[[kk]])), digits = digits )
        }
    }
  } # for
  invisible()
 } # prinE

prinP <- function(xs) {
  cat(xs, "\n")
  eval.parent(parse(text = xs))
  invisible()
} # prinP

prinV <- function(x, after = 2, before) {
  if (missing(before) || is.null(before))
    cat(paste(formatFix(x, after), collapse = ""), "\n")
  else
    cat(paste(formatFix(x, after, before), collapse = ""), "\n")
  invisible()
} # prinV

prinM <- function(x, after = 2, before) {
  res <- x
  res <- formatFix(x, after, before)
  dim(res) <- dim(x)
  print(res, quote = FALSE)
  invisible()
} # prinM

prinT <- function(x, rownam = FALSE, colnam = FALSE) {
  if (is.matrix(x)) {
    if (colnam) cat(paste(
        if (rownam) " ", paste(dimnames(x)[[2]], collapse = "\t"),
              sep = "\t"), "\n")
    for (ii in 1:dim(x)[1])
      cat(paste(
        if (rownam) dimnames(x)[[1]][ii],
                 paste(x[ii, ], collapse = "\t"),
                     sep = "\t"), "\n")
  }
  else {
    if (rownam) 
      cat(paste(names(x), collapse = "\t"), "\n")
    cat(paste(x, collapse = "\t"), "\n")
  }
  invisible()
} # prinT

NprinE <- function(xsv, ...) {
    cat("\n"); eval.parent(substitute(prinE(xsv, ...)))
}
NprinP <- function(xs) {
    cat("\n"); eval.parent(substitute(prinP(xs)))
}
NprinV <- function(x, after = 2,  before) {
    cat("\n"); prinV(x, after, before)
}
NprinM <- function(x, after = 2,  before) {
    cat("\n"); prinM(x, after, before)
}
NprinT <- function(x, rownam = FALSE,  colnam = FALSE) {
    cat("\n"); prinT(x, rownam, colnam)
}
