# Monte Carlo  simulation : Chance of casino losing money on roulette

#sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE) # 1000 draws from urn, -1 if red, else +1
X[1:10] # first 10 outcomes

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19)) # 1000 independent draws
S <- sum(x) # total winnings = sum of draws
S

#Monte carlo simulation to estimate probability of the csino losing money
n <- 1000 # number of roulette players
B <- 10000 # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19)) # simulate 1000 roulette spins
  sum(X) # determine total profit
})

mean(S < 0) # probability of the casino losing money

library(tidyverse)

s <- seq(min(S), max(S), length = 100) # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S=S) %>% # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10)+
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")
