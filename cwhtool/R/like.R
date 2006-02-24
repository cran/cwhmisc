like <- function(X,...){ 
    l <- list(...)
    n <- max(sapply(l, length))
    X <- X[rep(as.numeric(NA),n),]
    row.names(X) <- seq(length=n)
    for (nm in names(l)) X[[nm]][] <- l[[nm]]
    X
}

