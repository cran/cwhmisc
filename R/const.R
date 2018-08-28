c3Q <- .Machine$double.xmax ^ 0.75
c38 <- sqrt(c3Q)

GreatestIntAsRealF <- function() {
    p <- 0.0
    q <- 1.0
    while(p != q) {
      q <- q + q
      p <- q + 1.0
    }
    return(q / 2.0)
}  ## END  GreatestIntAsRealF;

ASCII <- c(NA, sapply(1:255, function(i) parse(text = paste("\"\\",
     structure(i, class = "octmode"), "\"", sep = ""))[[1]]));

HexDig <- c(0:9, LETTERS[1:6], letters[1:6])

HexagesDig <- c(0:9, LETTERS[1:25], letters[1:25])

EXPCHAR <- letters[26] #last( letters )

tau <- (1+sqrt(5))/2
