SplomT <- function (data,
           mainL = deparse(substitute(data)), xlabL = "", 
           hist = "h", adjust = 1,
           hist.col = trellis.par.get("strip.background")$col[5],
           cex.diag = 1, h.diag=0.4,
           colYonX = "red",
           colXonY = "blue", ...)
{
    force(mainL)
    stopifnot (hist %in% c("h", "d", "b"))
    datetime <- function() format(Sys.time(), "%Y-%m-%d, %X")
    data  <- data.frame(data)
    mxnam <- max(nchar(names(data)))
    lnam  <- ncol(data)
    ce    <- 100*cex.diag/lnam # *get.gpar()$cex/lnam
    cexd  <- ce/mxnam
    cexn  <- ce/5
    print(splom(~data, as.matrix = TRUE, main = mainL,
                xlab = paste(xlabL, 
                             "Blue: smothed in vertical, Red: smoothed in horizontal direction.\nHigh correlation corresponds to nearly parallel diagonal lines.\n",
                             datetime(),
                             sep = if (nchar(xlabL) > 0) ", " else " "),
       upper.panel = function(x, y, breaks = NULL, ...)
                {
                    minS <- 0.05
                    ccr <- cor(x, y, use = "complete.obs")
                    ccq <- sqrt(max(abs(ccr), minS))
                    if (is.na(ccr)) {ccr <- 0; ccq <- sqrt(minS)}
                    grid.text(round(ccr, 2), gp = gpar(cex = cexn*ccq))
                },
       lower.panel = function(x, y, ...,
                                       span = 2/3, degree = 1,
                                       family = "symmetric", evaluation = 50)
                {
                    panel.xyplot(x, y, pch = 3, cex = 1.5/dim(data)[2], ...)
                    lo <- try(loess.smooth(x, y, span = span, degree = degree,
                                           family = family, evaluation = evaluation))
                    if (!inherits(lo,"try-error"))
                        panel.lines(lo$x, lo$y, col.line = colYonX, ...)
                    lo <- try(loess.smooth(y, x, span = span, degree = degree,
                                           family = family, evaluation = evaluation))
                    if (!inherits(lo,"try-error"))
                        panel.lines(lo$y, lo$x, col.line = colXonY, ...)
                },
       diag.panel = function(x, varname, limits, ...)
                {
                    d <- density(x[!is.na(x)])
                    yrng <- range(d$y)
                    ylim <- yrng + 0.07 * c(-1, 1) * diff(yrng)
                    xlim <- current.panel.limits()$xlim
                    pushViewport(viewport(xscale = xlim, yscale = ylim))
                    if (hist %in% c("h", "b")) {
                        panel.histogram(x[!is.na(x)], breaks = NULL,
                                        col = hist.col, type = "density", ...)
                    }
                    if (hist %in% c("d", "b")) {
                        llines(d)
                    }
                    grid.text(varname,  y=unit(h.diag,"npc"), gp = gpar(cex = cexd))
                    popViewport()
                },
                varnames = abbreviate(names(data)), pscales = 0 )
          )
}  ## end SplomT
