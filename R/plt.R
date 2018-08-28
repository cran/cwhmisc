pltCharMat <- function(m, ...) {
  n22 <- n2cCompact(m)
  cm  <- charMat(n22)
  xl  <- extendrange(range(cm$x), f = 0.07)
  plot(xl, range(cm$y), type = "n")
  text(cm$x, cm$y, cm$tx, ...)
}  ## pltCharMat

pltRCT <- function (rows, cols, tit = "", f = function(x) 0, cex = 1.5,
   reset = TRUE, outer = TRUE, oma = c(2, 2, 4, 2), mar = c(4, 4, 2, 1)) {
      oldpar <- par(mfrow = c(rows, cols), oma = oma, mar = mar)
  if (reset)
      on.exit(par(oldpar))
  f
  L <- 1
  if (length(tit) > 1) {
      L <- 2
      mtext(tit[1], side = 3, line = 2,
            outer = outer, cex = cex, adj = 0.5)
  }
  mtext(tit[L], side = 3, line = 2 - L, outer = outer, cex = cex, adj = 0.5)
  mtext(datetime(), side = 1, line = 0, outer = outer, cex = 0.5, adj = 1)
  invisible()
}  # end pltRCT

histRCT <- function (data, rows = round(sqrt(ncol(data))), 
    cols = ceiling(ncol(data) / rows), breaks = "Sturges",
             mainl = deparse(substitute(data)),
             mainc = colnames(eval.parent(substitute(data)))) {
  if (is.null(dim(data))) {
     # "data" is vector
    if (!is.null(data)) {
      dt <- data
      dim(dt) <- c(length(data), 1)
      nc <- 1
    } else {
      nc <- 0
    }
 ## "data" is NULL
  } else {
# drop columns with less than 2 legal (non-NA) values
    dt <- data[, apply(data, 2, function(x) sum(!is.na(x)) > 1),
               drop = FALSE]
    nc <- ncol(dt)
  }
  if (nc > 0) {
    if (ncol(data) > nc) {
      rows <- round(sqrt(nc)); cols <- ceiling(nc / rows)
    }
    pltRCT(rows, cols, mainl, {
      for (ii in seq(nc)) {
        hist(dt[,ii], main = mainc[ii], xlab = "", breaks = breaks)
      }
    })
  }
  invisible()
} # end histRCT, 2008-06-02
