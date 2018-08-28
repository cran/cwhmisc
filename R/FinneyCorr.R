FinneyCorr <- function(s, n) {
  s2 <- s ^ 2
  return(exp(s2 / 2) * (1 - s2 * (s2 + 2) / (4 * n) + s2 ^ 2 * (3 *
           s2 ^ 2 + 44 * s2 + 84) / (96 * n ^ 2)))
}

FC.lm <- function(lmobj) {
  sumo <- summary.lm(lmobj, correlation = FALSE)
  return(FinneyCorr(sumo$sigma, length(sumo$residuals)))
}

R2.lm <- function(lmobj) {
  return(summary.lm(lmobj, correlation = FALSE)$r.squared)
}

s.lm <- function(lmobj) summary.lm(lmobj, correlation = FALSE)$sigma

summaryFs <- function(lmobj){
  print(summary(lmobj))
  prinE("FC.lm(lmobj)")
  prinE("s.lm (lmobj)")
} # summaryFs
