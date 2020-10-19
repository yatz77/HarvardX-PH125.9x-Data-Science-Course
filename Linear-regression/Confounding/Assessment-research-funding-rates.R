library(tidyverse)
library(dslabs)
library(broom)
data("research_funding_rates")
research_funding_rates
options(digits = 3)

awarded_by_gender <- research_funding_rates %>% 
  summarize(total_awarded_men = sum(awards_men), 
            total_awarded_women = sum(awards_women),
            total_not_awarded_men = sum(applications_men-awards_men),
            total_not_awarded_women = sum(applications_women-awards_women))
awarded_by_gender

two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two
two_by_two

two_by_two %>% summarize(percent_men_awarded = men[2]/sum(men), percent_women_awarded = women[2]/sum(women))

tidy(chisq.test(x = two_by_two$men, y = two_by_two$womem))

two_by_two %>% select(-awarded) %>% chisq.test()

?chisq.test

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat
research_funding_rates

dat %>% ggplot(aes(discipline, success, color = gender, size = applications )) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))
?xlab
