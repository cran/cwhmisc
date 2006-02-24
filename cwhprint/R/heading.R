heading <- function(text, comm = "", ch = "-", m = 1, n = 0)
{ # heading.s,   2001.01.02
	if(m > 0) cat(rep("\n", m))
	if(text != "") {
		cat(comm, text, "\n", sep = "")
		if(ch != "")
			cat(comm, paste(rep(ch, nchar(text)), collapse = ""), 
				"\n", sep = "")
	}
	if(n > 0)
		cat(rep("\n", n))
	invisible(NULL)
}
