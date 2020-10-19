set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") # if you are using R3.6 or later
library(HistData)
library(dslabs)
library(tidyverse)
data("GaltonFamilies")

female_heights <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)

mu_mothers <- mean(female_heights$mother)
sd_mothers <- sd(female_heights$mother)
mu_daughters <- mean(female_heights$daughter)
sd_daughters <- sd(female_heights$daughter)
mu_daughters
mu_mothers
sd_daughters
sd_mothers

rho_1 <- female_heights %>% summarise(r = cor(daughter, mother)) %>% pull(r)

m_1 <- rho_1 * sd_daughters/sd_mothers 
b_1 <- mu_daughters - m_1 * mu_mothers
m_1
b_1

rho_1^2

m_1*60+b_1


# Assesment 2 least squares estimates
model <- female_heights %>% lm(mother ~ daughter, data = .) 
predictions <- predict(model)
predictions
female_heights %>% lm(mother ~ daughter, data = .) %>% 
female_heights[1,2]%>%pull()
