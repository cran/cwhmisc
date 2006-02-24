# Agustin Lobo <alobo@ija.csic.es> writes:
# 
# > I am now using a for loop and applying
# > union() to each pair of vectors, but is
# > there a faster way avoiding the for ?
# 
# Not faster, I think. Maybe neater, using something like
# 
# lapply(seq(along=l1), function(i)union(l1[[i]],l2[[i]]))
# 
# or (with napply from an earlier post of mine)
# 
# napply(l1,l2,FUN=union)
# 
# where
# 
napply <- function(..., FUN) {
   x <- list(...)
   lens <- sapply(x,length)
   len <- max(lens)
   if (any(lens != len)) x <- lapply(x, rep, length=len)
   tuples <- lapply(seq(length=len), function(i)lapply(x,"[", i))
   lapply(tuples, function(t)eval(as.call(c(FUN,t))))
}
