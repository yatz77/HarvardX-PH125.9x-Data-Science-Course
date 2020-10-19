library(dslabs)
library(dplyr)
library(tidyverse)
library(lubridate)
library(caret)
data("reported_heights")
reported_heights
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 01)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass", "online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
x
# accuracy of predicting female or male based on type (online or inclass)
dat %>% filter(type == "inclass") %>% summarise(mean(sex == "Female"))
dat %>% filter(type == "online") %>% summarise(mean(sex == "Female"))
dat %>% summarise(mean(type == "inclass"))
y_hat <- dat %>% mutate(y_hat = ifelse(type == "inclass","Female","Male"))%>%select(y_hat)%>%pull()
y_hat <- as.factor(y_hat)
class(y_hat)
class(y)
mean(y_hat == y)

# confusion matrix
table(y_hat, y)
#sensitivity
sensitivity(y_hat, y)
#specificity
specificity(y_hat, y)
#prevalence females
mean(y == "Female")
