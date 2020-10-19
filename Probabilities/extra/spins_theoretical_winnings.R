library(tidyverse)

win <- 1
lose <- 0
edge <- 0.34
n_games <- 100000000

n_wins <- sample(c(win,lose),n_games, replace = T, prob = c(edge, 1-edge))
multiplier <- sample(c(2,3,5,10,25,120,240,12000),n_games, replace = T, prob = c(0.494882,0.414012,0.085,0.005,0.001,0.000075,0.000030,0.000001))
total_winnings <- sum(multiplier*n_wins)
total_winnings # after n_games of one dollar spin and gos
net_winnings <- total_winnings - n_games
net_winnings
B <- 25

mc_net_winnings <- replicate(B, {
  n_wins <- sample(c(win,lose),n_games, replace = T, prob = c(edge, 1-edge))
  multiplier <- sample(c(2,3,5,10,25,120,240,12000),n_games, replace = T, prob = c(0.494882,0.414012,0.085,0.005,0.001,0.000075,0.000030,0.000001))
  total_winnings <- sum(multiplier*n_wins)
  net_winnings <- total_winnings - n_games
  net_winnings
  })

# at least an edge of 0.37 is needed to be profitable at spin and go's

data.frame(mc_net_winnings = mc_net_winnings) %>%
  ggplot(aes(mc_net_winnings)) +
  geom_density(color="black")+
  xlim(-10000000, 5000000)


