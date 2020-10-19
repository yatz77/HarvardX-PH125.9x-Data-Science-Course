# suggested libraries and options
library(tidyverse)
options(digits = 3)

#load brexit_polls object
library(dslabs)
data("brexit_polls")

#Final Brexit parameters
p <- 0.481 # official proportion voting "Remain"
d <- 2*p-1 # official spread

#sample of poll 1500 voters
N <- 1500
p*N # expected total number of voter in the sample
sqrt(N)*sqrt(p*(1-p)) # standard error of total number of voters
sqrt(p*(1-p)/N) # standard error of proportion of Remain voters
d <- 2*p-1 # expected value of d
2*sqrt(p*(1-p)/N) # standard error of d

#Analysis brexit polls
head(brexit_polls)
brexit_polls <- brexit_polls %>% mutate(x_hat =(spread+1)/2) # addition of fraction remain
brexit_polls %>% summarize(mean(spread), sd(spread), mean(x_hat), sd(x_hat)) # average of the observed spreads

#Confidence intervals for Brexit polls
brexit_polls[1,] %>% summarize(x_hat - qnorm(0.975) *sqrt(x_hat*(1-x_hat)/samplesize)) # lower bound 95% confidence interval
brexit_polls[1,] %>% summarize(x_hat + qnorm(0.975) *sqrt(x_hat*(1-x_hat)/samplesize)) # upper bound 95% confidence interval

#June polls: confidence intervals that contain the real result
june_polls <- brexit_polls %>% filter(enddate > "2016-06-01")
june_polls <- june_polls %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), 
                                    se_spread = 2* se_x_hat, lower = spread - qnorm(0.975)*se_spread, 
                                    upper = spread + qnorm(0.975)*se_spread)
june_polls <- june_polls %>% mutate(hit = lower < d & upper > d)
june_polls %>% summarize(mean(lower<0 & upper >0)) # proportion of confidence intervals containing 0
june_polls %>% summarize(mean(lower>0 & upper >0)) # proportion of confidence intervals entirely above 0

june_polls %>% summarize(mean(hit)) # proportion of confidence intervals containing d

#pollster to pollster variability

june_polls %>% group_by(pollster) %>% summarize(n= n(), hits = mean(hit)) %>% arrange(desc(hits))

one_poll <- sample(c(1,0), N, replace = TRUE, prob = c(p,1-p))


#Boxplot of Brexit polls by poll type

june_polls %>% ggplot(aes(x = poll_type, y = spread)) + geom_boxplot() + geom_point()

#Combined spread across poll type

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
                 spread = sum(spread*samplesize)/N,
                 p_hat = (spread + 1)/2)
conf_combined <- combined_by_type %>% 
  group_by(poll_type)%>%
  summarize(lower = spread - qnorm(0.975) * 2*sqrt(p_hat*(1-p_hat)/N),
            upper = spread + qnorm(0.975) * 2*sqrt(p_hat*(1-p_hat)/N)) # confidence intervals for total per type

#Chi-squared p-value

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
brexit_hit

two_by_two_brexit <- brexit_hit %>% 
  group_by(poll_type, hit) %>% 
  summarize(num = n()) %>% 
  spread(poll_type, num)
two_by_two_brexit

chisq_test_brexit <- chisq.test(two_by_two_brexit)
chisq_test_brexit$p.value

# which poll type has a higher probability of having a confidence interval covering the
#correct value of the spread

hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05

#odd ratio

ratio_hit_online <- two_by_two_brexit[2,2]/(two_by_two_brexit[2,2]+two_by_two_brexit[1,2])/(two_by_two_brexit[1,2]/(two_by_two_brexit[2,2]+two_by_two_brexit[1,2]))
ratio_hit_online
ratio_hit_telephone <- two_by_two_brexit[2,3]/(two_by_two_brexit[2,3]+two_by_two_brexit[1,3])/(two_by_two_brexit[1,3]/(two_by_two_brexit[2,3]+two_by_two_brexit[1,3]))
ratio_hit_telephone

odd_ratio <- ratio_hit_online/ratio_hit_telephone
odd_ratio

# spread over time

brexit_polls %>% 
  ggplot(aes(x = enddate, y = spread, color = poll_type)) + 
  geom_smooth(method = "loess", span = 0.4) +
  geom_point()

# plotting raw percentages over time

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(x = enddate, y = proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3) +
  geom_point()

