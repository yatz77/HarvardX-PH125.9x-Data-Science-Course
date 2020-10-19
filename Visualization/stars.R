library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)

mean(stars$magnitude)
sd(stars$magnitude)

#density plot of magnitude
stars %>% ggplot(aes(magnitude)) +
  geom_density()

#distribution star temperature
stars %>% ggplot(aes(temp)) +
  geom_histogram()

# scatterplot temperature versus magnitude
stars %>% ggplot(aes(temp, magnitude)) +
  geom_point()

# flipped axes scatterplot
stars %>% ggplot(aes(temp, magnitude)) +
  geom_point() + 
  scale_y_reverse() +
  scale_x_continuous(trans = "log10") +
  scale_x_reverse()

# name stars
stars %>% ggplot(aes(temp, magnitude, label = star)) +
  geom_point() + 
  scale_y_reverse() +
  scale_x_continuous(trans = "log10") +
  scale_x_reverse() +
  geom_text_repel()

# color stars
stars %>% ggplot(aes(temp, magnitude, color = type)) +
  geom_point() + 
  scale_y_reverse() +
  scale_x_continuous(trans = "log10") +
  scale_x_reverse() 

