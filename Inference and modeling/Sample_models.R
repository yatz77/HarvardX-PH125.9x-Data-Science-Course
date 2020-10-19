# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>% # keep latest poll
  ungroup()
one_poll_per_pollster
