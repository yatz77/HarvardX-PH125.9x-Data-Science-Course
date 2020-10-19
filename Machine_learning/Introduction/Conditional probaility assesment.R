#Assesment 1: disease
library(dslabs)
library(tidyverse)
set.seed(1, sample.kind = "Rounding")
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
mean(test == 0)
mean(disease[test == 0]==1)
mean(disease[test == 1]==1)/0.02

#Assesment 2: heights

# plot conditional probability of being male for all heights rounded to an inch
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data = .)

# same plot but now using quantiles to assure the same amount of information
ps <- seq(0, 1, 0.1)
heights %>%
  mutate(g = cut(height, quantile(height, ps), include.lowest = T)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data=.)

# generate data from a bivariate normal distribution using the MASS package
install.packages("MASS")
library(MASS)
sigma <- 9*matrix(c(1,0.5,0.5,1),2,2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
sigma
dat
plot(dat)
?mvrnorm

# make conditionalporbability plot using the random normal covariate data
ps <- seq(0, 1, 0.1)
dat %>% mutate(g = cut(x, quantile(x, ps), include.lowest = T)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data = .)
