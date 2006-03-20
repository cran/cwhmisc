summaryFs <- function(lm){
##    "prinE" <- function(xsv, ...) { 
##    cat(xsv, "=")
##    print(as.vector(eval.parent(parse(text = xsv))), ...)
##    invisible(xsv)
##  }
  print(summary(lm))
  prinE("FC.lm(lm)")
  prinE("s.lm (lm)")
}
