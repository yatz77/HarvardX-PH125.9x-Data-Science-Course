library(tidyverse)
library(dslabs)
data(murders)

murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))
 # claculate Us murder rate, generating a data frame
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) *100000)
us_murder_rate

# extract the numeric us murder rate with the dot operator
us_murder_rate %>% pull(rate)

# claculate and extract the murder rate with one pipe
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  .$rate
s <- (sum(murders$total) / sum(murders$population) *100000)
s
