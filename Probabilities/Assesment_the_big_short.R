options(digits = 3)
library(tidyverse)
library(dslabs)
data("death_prob")
head(death_prob)

# Question 1: probabilit< losing money on policies on 50 year old females
p_f50<-death_prob %>% filter(age == 50, sex == "Female") %>% pull(prob) #probability a 50 year old woman dies
q_f50<-1-p_f50
a_f50<--150000
b_f50 <- 1150
n <- 1000

exp <- p_f50*a_f50+q_f50*b_f50 # expected value insurance company 50 year old woman
std_error <- abs(a_f50-b_f50)*sqrt(p_f50*q_f50)
std_error
exp_1000 <- n*exp # exp value 1000 policies 50 year old woman
std_error_1000 <- sqrt(n)*std_error
pnorm(0,exp_1000,std_error_1000) # chance company loses money
 
#Question 2: now 50 year old males

p_m50<-death_prob %>% filter(age == 50, sex == "Male") %>% pull(prob) #probability a 50 year old woman dies
q_m50<-1-p_m50
a_m50<--150000
profit_m50 <- 700000
n <- 1000

b_m50 <- (profit_m50 - n*a_m50*p_m50)/(n*q_m50) # how much premium to ask to make 700000 profit
std_error_1000_m50 <- sqrt(n)*abs(a_m50-b_m50)*sqrt(p_m50*q_m50)
pnorm(0,profit_m50,std_error_1000_m50) # probability losing money


#Question 3: pandemy
a <- -150000
b <- 1150
p <- 0.015
q <- 1-p
n <- 1000
exp <- n*((1-p)*b + a*p)
std_error <- sqrt(n)*abs(a-b) *sqrt(p*(1-p))
pnorm(0,exp,std_error)
pnorm(-1000000,exp,std_error) # probability company loses more than a million in case of pandemy

p <- seq(.01, .03, .001)  # different death probabilities
losing_money <- pnorm(0,exp,std_error)
min(p[which(losing_money>0.9)]) # minimum death probability where losing money exceeds 90%

p <- seq(.01, .03, .0025)
exp <- n*((1-p)*b + a*p)
std_error <- sqrt(n)*abs(a-b) *sqrt(p*(1-p))
losing_money <- pnorm(-1000000,exp,std_error)
min(p[which(losing_money>0.9)])

# sampling model simulating total profit in case of pandemy
set.seed(25, sample.kind ="Rounding")
n <- 1000
p_loss <- 0.015
a <- -150000
q_loss <- 1-p_loss
b <- 1150
B <- 1

S <- replicate(B, {
  s <- sample(c(a,b),n,replace = TRUE, prob = c(p_loss,1-p_loss))
  sum(s)
})
S/1000000

set.seed(27, sample.kind ="Rounding")
n <- 1000
p_loss <- 0.015
a <- -150000
q_loss <- 1-p_loss
b <- 1150
B <- 10000

S <- replicate(B, {
  s <- sample(c(a,b),n,replace = TRUE, prob = c(p_loss,1-p_loss))
  sum(s)
})
hist(S)
mean(S <= -1000000)

#new premium under pandemic to avoid losing money with a prob of 5%
#to calculate premium go to z-score(equation in section 4.1)
a <- -150000
p <- 0.015
q <- 1-p
z <- qnorm(0.05)
n <-1000

b <- -a*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p))) #new premium
exp <- a*p+b*q
exp_1000 <- exp*n

#Monte carlo probability losing money on 1000 policies with new premium with pandemic
set.seed(28, sample.kind = "Rounding")
B <- 10000

S <- replicate(B, {
  s <- sample(c(a,b),n,replace = TRUE, prob = c(p,q))
  sum(s)
})

mean(S < 0)

#Monte Carlo for unstable pandemic death rate
set.seed(29, sample.kind = "Rounding")
p <- 0.015
a <- -150000
b <- 3268
B <- 10000
S <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  s <- sample(c(a,b),n,replace = TRUE, prob = c(new_p,1-new_p))
  sum(s)
})
mean(S) #expected value
hist(S)
mean(S < 0) # prbability of losing money
mean(S < -1000000)
