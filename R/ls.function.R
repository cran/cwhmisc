ls.functions <- function() {
   x <- eval.parent(quote(ls()))
   x[sapply(x, function(x) typeof(get(x)) == "closure")]
}

ls.notfunctions <- function() {
   x <- eval.parent(quote(ls()))
   x[sapply(x, function(x) typeof(get(x)) != "closure")]
}

