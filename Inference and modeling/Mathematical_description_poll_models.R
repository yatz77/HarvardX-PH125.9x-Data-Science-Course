# Code: Simulated data with Xj = d + ej

J <- 6
N <- 2000
d<- .021
p<- (d+1)/2
X <- d+ rnorm(J, 0, 2*sqrt(p*(1-p)/N))

# Code: Simualted data with Xi,j = d + ei,j

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0 , 2*sqrt(p*(1-p)/N))
})

# Code: Simulated data with Xi,j = d + hi + ei,j

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025) # assume dtandard error of pollster-to-pollster variabilit< is 0.025
X <- sapply(1:I, function(i){
  d + h[I] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

# Code: Calculating probability of d>0 with general bias

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/ (1/sigma^2 + 1/tau^2))

1-pnorm(0, posterior_mean, posterior_se)

