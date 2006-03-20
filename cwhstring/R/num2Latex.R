num2Latex <- function (x, digits = 0) {
     op <- options(scipen = -digits)
     on.exit(options(op))
     x <- as.character(x)
     ind <- grep("e", x)
     x[ind] <- sapply(strsplit(x[ind], "e"), function(s) paste(s[1], 
 "e", as.numeric(s[2]), sep = ""))
     x[ind] <- paste(gsub("e", " \\cdot 10^{", x[ind], fixed = TRUE), 
 "}", sep = "")
     x
}  # num2Latex
