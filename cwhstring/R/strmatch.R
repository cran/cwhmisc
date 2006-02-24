strmatch <- function( inputs, target) {
    if (!(is.character(inputs) && is.character(target)))
	stop ("Input must be character strings")

    if (length(inputs) <1) return(NULL)
    if (length(target) <1) return(rep(NA, length(inputs)))

    temp <- .C("strmatch", inputs, length(inputs), target, length(target),
		   result=integer(length(inputs)),PACKAGE="base")

    ifelse(temp$result<0, NA, temp$result)
}
