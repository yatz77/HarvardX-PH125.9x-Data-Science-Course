# interest rate per sampling model

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# interest rate Monte Carlo simulation

B <- 10000
losses <- replicate(B, {
 defaults <- sample(c(0,1), n, prob=c(1-p, p), replace = TRUE)
 sum(defaults * loss_per_foreclosure)
})


# plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

#Expected value and standard error of the sum of 1000 loans
n*(p*loss_per_foreclosure + (1-p)*0) # expected value
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p)) # standard error

# Interest rates for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*(n*p-z*sqrt(n*p*(1-p)))/ (n*(1-p) + z*sqrt(n*p*(1-p)))
x # required profit when loan is not a foreclosure
x/180000 # interes rate
loss_per_foreclosure*p + x*(1-p) # expected value of the profit per loan

n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#Monte Carlo simulation for 1% probability of losiing money

B <- 100000
profit <- replicate(B, {
  draws <- sample(c(x,  loss_per_foreclosure), n,
                  prob=c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit) # expected value of the profit over n loans
mean(profit<0) # probability of losing money


# Expected vlaue woth higher default rate and interest rate
p <- 0.04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r *180000
loss_per_foreclosure*p + x*(1-p)

# calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n # number of loans required
n*(loss_per_foreclosure*p + x *(1-p)) # expected profit over n loans

# Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 *180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n,
                   prob = c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit)

data.frame(profits = profit/10^6) %>%
  ggplot(aes(profits)) +
  geom_histogram(binwidth = 0.6, col = "black")

# Monte Carlo simulation with unknown default probability
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100),1)
  draws <- sample(c(x, loss_per_foreclosure), n,
                prob = c(1-new_p, new_p), replace = TRUE)
sum(draws)
})
mean(profit) # expected profit
mean(profit < 0) # probabilit< of losing money
mean(profit < -10000000) # probability of losing over 10 million
