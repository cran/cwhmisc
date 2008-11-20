".First.lib" <- function(lib, pkg) {
  require("lattice")
spacC <- "  "  ## extra space in indexLine and charMat

ASCII <- c(NA, sapply(1:255, function(i) parse(text=paste("\"\\", structure(i,class="octmode"), "\"", sep=""))[[1]]));

HexDig <- c('0','1','2','3','4','5','6','7','8','9',LETTERS[1:6])

caplow.ff <- function(str, f) { # used in capitalize and lowerize
  x <- strsplit(str, NULL)
  y <- unlist(x)
  z <- lapply(y, f)
  paste(z, collapse="")
}
}
