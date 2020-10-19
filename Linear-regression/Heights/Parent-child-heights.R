# ASSESMENT". TIBBLES, DO AND BROOM
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>%
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% group_by(pair)%>%summarize(r = cor(childHeight, parentHeight))

galton %>% 
  group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>%
  filter(term == "parentHeight")
  