"div.prot" <- function(x, eps = .Machine$double.eps)
{
        index <- abs(x/max(abs(x))) > eps
        result <- x
        result[index] <- 1/result[index]
        result[!index] <- 0
        result
}
