library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

 results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes) # top 5 states
 
 #Code: Computing the average and standard deviation for each state
 
 results <- polls_us_election_2016 %>%
   filter(state != "U.S." &
            !grepl("CD", "state") &
            enddate >= "2016-10-31" &
            (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
   mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
   group_by(state) %>%
   summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
   mutate(state = as.character(state))
 
# 10 closest races = battleground states
 results %>% arrange(abs(avg))
 help(grepl)  

 # joining electoral college votes and results
 results <- left_join(results, results_us_election_2016, by="state")

 # states with no polls: note Rhode Island and District of Columbia = Democrat
 results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd tp states with just one poll as median of othher sd values
 results <- results %>%
   mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

# Calculating the posterior mean and posterior standard eroor
 
 mu <- 0
 tau <- 0.02
 results %>% mutate(sigma = sd/sqrt(n),
                    B = sigma^2/ (sigma^2 + tau^2),
                    posterior_mean = B*mu + (1-B)*avg,
                    posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
   arrange(abs(posterior_mean))
 head(results)
 # Code: Monte Carlo simulation of elction night results (no general bias)
 mu <- 0
 tau <- 0.02
 clinton_EV <- replicate(1000, {
   results %>% mutate(sigma = sd/sqrt(n),
                      B = sigma^2/ (sigma^2 + tau^2),
                      posterior_mean = B*mu + (1-B)*avg,
                      posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                      simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                      clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
     summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
     .$clinton + 7    # 7 votes for Rhode Island and DC
 })

mean(clinton_EV > 269) # over 269 votes wins election 
clinton_EV

# Code: Monte Carlo simulationincluding general bias

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2), # added bias_sd term
                     B = sigma^2/(sigma^2 + tau^2),
                       posterior_mean = B*mu + (1-B)*avg,
                       posterior_se = sqrt(1/ (1/sigma^2 + 1/tau^2)),
                       simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                       clinton = ifelse(simulated_result >0, electoral_votes, 0)) %>% # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>% # total votes for Clinton
    .$clinton + 7 # 7 votes for Rhode Island and DC
  })
mean(clinton_EV_2 > 269) # over 269 votes wins election

