# define x as vector of male heights
library(tidyverse)
library(dslabs)
data("heights")
index <- heights$sex=="Male"
x <- heights$height[index]
index

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x-average)^2)/length(x))
SD2 <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)
