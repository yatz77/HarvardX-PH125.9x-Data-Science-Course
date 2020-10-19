library(dslabs)
data("heights")
library(tidyverse)
class(heights)
class(heights$sex)
class(heights$height)
class(heights§sex[1])
heights$sex[1]

nrow(heights)
heights$height[777]
heights$sex[777]
heights[1, 777]
heights[777,1]
max(heights$height)
which.min(heights$height)
mean(heights$height)
median(heights$height)
heights %>% gather()
heights
heights %>% spread(height, sex)
n_male <- heights%>% filter(sex == "Male") %>% nrow() 
n_total <- heights %>% nrow()
n_male
n_total
heights %>% filter(height > 78) %>% nrow()
heights %>% filter(sex == "Female", height > 78) %>% nrow()
