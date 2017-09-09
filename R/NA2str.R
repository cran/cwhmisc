NA2str <- function( x ) {
    ifelse( is.na (x) & !is.nan (x), " NA", as.character(x) )
} # NA2str
