n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m
 set.seed(1995, sample.kind = "Rounding")
N <- 250
X <- sample(income, N)
M <- median(X)
M

# now perform a Monte Carlo simulation to estimate the distribution of incomes
library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) +geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

# Estimate the distribution using the bootstrap method
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = T)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) +
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96*sd(X)/ sqrt(N) * c(-1, 1)

mean(M) + 1.96* sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1,1)

#COMPREHENSION CHECK

# create bootstrap sample indices for mnist 27 dataset
set.seed(1995, sample.kind = "Rounding")
indexes <- createResample(mnist_27$train$y, 10)


sum(indexes$Resample01 == 3)

# check how many times a certain index returns in all 10 bootstrap samples
index_df <- as.data.frame(indexes)
sum(as.matrix(index_df) == 3)

# estimate the expected value of the 0.75th quantile with a standard normal dataset using Monte Carlo 
y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)

set.seed(1, sample.kind = "Rounding")

B <- 10^5
seventyfifth_quantiles <- replicate(B, {
  S <- rnorm(100, 0, 1)
  quantile(S, 0.75)
})
mean(seventyfifth_quantiles_star)
sd(seventyfifth_quantiles_star)

# estimate the expected value of the 0.75th quantile with a standard normal dataset using the bootstrap method
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
ind <- createResample(y, 10)
seventyfifth_quantile_star <- sapply(ind, function(x) {
  quantile(y[x], 0.75)
})
mean(seventyfifth_quantile_star)
sd(seventyfifth_quantile_star)

# now with 10000 bootstrap samples instead of 10
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
ind <- createResample(y, 10000)
seventyfifth_quantile_star <- sapply(ind, function(x) {
  quantile(y[x], 0.75)
})
mean(seventyfifth_quantile_star)
sd(seventyfifth_quantile_star)
