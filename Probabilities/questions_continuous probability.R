library(dslabs)
library(tidyverse)

#Question 1: act scores

set.seed(16, sample.kind = "Rounding")

act_scores <- rnorm(10000, 20.9, 5.7) # simulated act scores
mean(act_scores)
sd(act_scores)
mean(act_scores >= 36)*10000
mean(act_scores >= 30)
mean(act_scores <= 10)

# question 2
help(dnorm)
x <- seq(1,36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x,f_x)

# question 3: z scores
z_scores <- (act_scores-mean(act_scores))/sd(act_scores)
mean(z_scores > 2)
20.8 + 2*5.68
qnorm(0.975, 20.8, 5.68)

# question: cdf z scores


f_t <- function(t) {dnorm(t, 20.8, 5.68)}
class(f_t)
test <- function(a){
  integrate(f_t,-Inf, a)
}
test(30)

cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))
cdf

qnorm(0.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
q <- quantile(p)
sample_quantiles <- quantile(act_scores,p)
names(sample_quantiles[max(which(sample_quantiles < 26))])
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles,sample_quantiles)
