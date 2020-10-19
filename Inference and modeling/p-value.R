# Code. Computing a p-value for observed spread of 0.04
N <- 100 # sample size
z <- sqrt(N) * 0.02/0.5 # spread of 0.04
1-(pnorm(z) - pnorm(-z))
