library(tidyverse)
library(dslabs)

data(co2)

head(co2)
co2

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = T)) %>%
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide, month, co2, -year)
co2_tidy

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()


data("admissions")

head(admissions)

admissions

dat <- admissions %>% select(-applicants)

head(dat)

dat2 <- dat %>% spread(major, admitted) %>% gather(major, admitted, -gender) %>% spread(gender, admitted)
dat2

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, key, gender)
tmp2
tmp3 <- spread(tmp2,column_name, value)
tmp3
