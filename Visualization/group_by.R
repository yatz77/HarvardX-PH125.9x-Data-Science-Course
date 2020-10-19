# libraries and data
library(tidyverse)
library(dslabs)
data(heights)
data(murders)

# compute separate aveerage and standard deviation for male/female heights
heights %>% 
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# comüute median murder rate in 4 regions of country
murders <- murders %>%
  mutate(murder_rate = total/population * 100000)
murders %>%
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))
heights %>% group_by(sex)
murders %>% group_by(region)

