p.screeplot.princomp <- function (object, variables = seq(length(object$sdev)), cumulative = TRUE, main = deparse(substitute(object)), ylim = c(0, max(vars) + 0.05), ...) 
{
    len <- length(variables)
    nn <- min(10, len)
    nsmal <- 3
    if (length(names(object$sdev)) > 4) {
        names(object$sdev) <- substring(names(object$sdev), 7)
        nsmal <- 2
    }
    variables <- variables[1:nn]
    vars <- object$sdev^2
    vars <- vars[variables]/sum(vars)
    bp <- barplot(vars, main = main, ylim = ylim, ...)
    cs <- round(cumsum(vars), nsmal)
    cs <- ifelse(cs == 1, "1", substring(cs, 2))
    if (cumulative) 
        text(bp, vars + par("cxy")[2], cs, cex = 0.7)
    invisible(bp)
}
