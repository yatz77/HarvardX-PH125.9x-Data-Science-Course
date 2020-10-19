library(dslabs)
library(tidyverse)


#Question 1: SAT test part 1
n <- 44 # number of questions
std_error <- sqrt(n)*abs(1+0.25)*sqrt(0.2*0.8) # standard error on guessing on all questions
avg <- 0
1-pnorm(8,avg,std_error) # probability guessing student gets a score of 8 or higher
# Monte Carlo simulation guessing student
set.seed(21, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  s <- sample(c(1, -0.25), n, replace = TRUE, prob = c(0.2,0.8))
  sum(s)
})
mean(S>8)
S[1:10]

#Question 2: SAT part 2
# Now only 4 questions and no negative points
a <- 1
b <- 0
p <- 0.25
q <- 0.75

avg <- n*(p*a+((1-p)*b)) # expected value guessing all questions
avg
std_error <- sqrt(n)*abs(1+0)*sqrt(p*(1-p)) # standard error on guessing on all questio
std_error
1-pnorm(30,avg,std_error) # probabilit< higher than 30

#determine for which p the probability of a score higher than 35 is 80%
p <- seq(0.25, 0.95, 0.05)
q <- 1-p
avg <- n*(p*a+((1-p)*b)) # expected value guessing all questions
avg
std_error <- sqrt(n)*abs(1+0)*sqrt(p*(1-p)) # standard error on guessing on all questio
std_error
prob_o35 <- 1-pnorm(35,avg,std_error) # probability higher than 35
data_o35 <- data.frame(p=p,probability_score_over_35 = prob_o35)
data_o35 %>% filter(prob_o35 > 0.8) %>% pull(p) 
min(p[which(prob_o35 > 0.8)])
help(which)


#Question 3: Betting roulette
a <- 6
b <- -1
p <- 5/38
q <- 1-p

expected <- a*p+b*q
std_error <- abs(a-b)*sqrt(p*q)
exp_500 <- expected #expected value of the average payout over 500 bets
std_error_500 <- std_error/sqrt(500) #expected value of the standard error of the average payout over 500 bets
exp_sum_500 <- expected*500
std_error_500_sum <- std_error*sqrt(500)
pnorm(0,exp_sum_500,std_error_500_sum) # probabilit< of losing money
