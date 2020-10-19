# Code: Confidence intercal for the spread with sample size of 25

N <- 10000
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)
