a simple scatterplot of total murders versus population
x <- murders$population / 10^6
y <- murders$total
plot(x,y)

rate <- murders$total/murders$population*100000
# a histogram of a murder rates
hist(murders$rate)

# boxplots of nurder rates by region
boxplot(rate~region, data = murders)
j