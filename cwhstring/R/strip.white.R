strip.whites <- function(string) {
  # strip.whites.s  agw  9/3/96  Chuck Lo Presti  Statistics Group Energy Division Pacific Northwest National Laboratory Richland WA 99352 d38716@gaviota.pnl.gov
  string <- system("sed 's/^\ *//'",string)
  string <- system("sed 's/\ *$//'",string)
  return(string)
}
