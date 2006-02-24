#################################################
#
# need to add parameter for minimum expected value and code to collapse tails
#
# combine this with a poisson routine and a greens index routine that
# returns a list for adding to a summary data frame ...
#################################################


neg.bin.gof <- function(data)
  {
  
  case.count <- vector(mode = "numeric", length = max(data) + 2)
  observed <- vector(mode = "numeric", length = max(data) + 2)
  neg.bin.expected <- vector(mode = "numeric", length = max(data) + 2)
  neg.bin.partial.chi.square <- vector(mode = "numeric", length = max(data) + 2)
  
  #
  # calculate frequencies
  #
  for (i in 0:(max(data) + 1))
    {
    case.count[i + 1] <- i
    observed[i + 1] <- sum(data == i)
    }  
 
  #
  # calculate summary measures
  #
  N.SU <- length(data)
  cases <- sum(data)
  cases.per.SU.mean <- cases / N.SU
  cases.per.SU.var <- var(data)
  
  #
  # find negative binomial distribution parameter 'k' by successive approximation
  #
  delta.direction <- 0
  delta.difference <- 1
  k <- cases.per.SU.mean ^ 2 / (cases.per.SU.var - cases.per.SU.mean)
  repeat  
    { 
    difference <- log10(N.SU / observed[1]) - k * log10(1 + (cases.per.SU.mean / k))
    if(abs(difference) < 0.0000001) break
    if (delta.direction != sign(difference)) delta.difference <- delta.difference / 10
    delta.direction <- sign(difference)
    k <- k + delta.direction * delta.difference
    }  

  #
  # calculate expected number of sampling units with ZERO cases (NEGATIVE BINOMIAL)
  #
  neg.bin.expected[1]  <- (1 + (cases.per.SU.mean / k)) ^ (0 - k) * N.SU
  neg.bin.sum.expected <- neg.bin.expected[1]
  
  #
  # calculate expected number of sampling units with 1, 2, etc. cases (NEGATIVE BINOMIAL)
  #
  for (i in 2:(max(data) + 1))
    {
    neg.bin.expected[i] <- (cases.per.SU.mean / (cases.per.SU.mean + k)) * ((k + i - 1 - 1) / (i - 1)) * neg.bin.expected[i - 1]
    neg.bin.sum.expected <- neg.bin.sum.expected + neg.bin.expected[i]
    } 
  neg.bin.expected[max(data) + 2] <- N.SU - neg.bin.sum.expected  

  #
  # calculate chi-square statistic (NEGATIVE BINOMIAL)
  #
  neg.bin.partial.chi.square <- (observed - neg.bin.expected) ^ 2 / neg.bin.expected
  neg.bin.chi.square.statistic <- sum(neg.bin.partial.chi.square)  

  #
  # some output
  #
  output.table <- cbind(case.count, observed, neg.bin.expected, neg.bin.partial.chi.square)
  print(output.table)  
  print(neg.bin.chi.square.statistic)
  print(k)
}
 

