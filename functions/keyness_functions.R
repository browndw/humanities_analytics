
# This does a basic log-likelihood test (also called a goodness-of-fit test).
# It calcultaes the observed vs. the expected frequencies.
# Note the expected frequencies account for the relative proportions of each corpus.

log_like <- function(frequency_a, frequency_b, total_a, total_b) { 
  expected_a <- total_a*((frequency_a+frequency_b)/(total_a+total_b))
  expected_b <- total_b*((frequency_a+frequency_b)/(total_a+total_b))
  L1 <- if(frequency_a == 0) 0 else (frequency_a*log(frequency_a/expected_a))
  L2 <- if(frequency_b == 0) 0 else (frequency_b*log(frequency_b/expected_b))
  likelihood <- 2*(L1 + L2)
  likelihood <- round(likelihood, 2)
  return(likelihood)
}

# Take a target value and a reference value, and return an effect size
# This effect size calcuation is called Log Ratio
# And was proposed by Andrew Hardie: http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/

log_ratio <- function(frequency_a, frequency_b, total_a, total_b) { 
  percent_a <- if(frequency_a == 0) (.5/total_a) else (frequency_a/total_a)
  percent_b <- if(frequency_b == 0) (.5/total_b) else (frequency_b/total_b)
  ratio <- (log2(percent_a/percent_b))
  ratio <- round(ratio, 2)
  return(ratio)
}

