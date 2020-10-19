# load heights data
library(tidyverse)
library(dslabs)
data(heights)

# define p
p <- heights %>%
  filter(sex =="Male") %>%
  ggplot(aes(x = height))

# basic histogram
p+ geom_histogram()
p+geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill  = "blue", col = "black") +
  xlab("Male heights in inches")+
  ggtitle("Histogram")

p+geom_density()
p+geom_density(fill = "blue")


# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()


# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
                     
# arrange plots next to each other in 1 row, 3 columns
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
