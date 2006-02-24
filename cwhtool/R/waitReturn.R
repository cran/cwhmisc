waitReturn <- function(ask=TRUE) {
  if (ask & interactive() & sink.number()==0) readline("\nType  <Return>\t to continue : ")
  invisible()
}
