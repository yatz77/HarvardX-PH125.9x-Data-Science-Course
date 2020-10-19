X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25) # estimate of standard error
pnorm(0.01/se)-pnorm(-0.01/se) # probabilit x-hat being within .01 of p