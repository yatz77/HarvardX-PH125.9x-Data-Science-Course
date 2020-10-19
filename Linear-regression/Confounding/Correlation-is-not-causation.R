# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N*g), y = rnorm(N*g))

# calculate correlation between x,y for each group
res <- sim_data %>%
  group_by(group) %>%
  summarize(r = cor(x,y)) %>%
  arrange(desc(r))
res

#plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x,y)) +
  geom_point() +
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# linear regression on gorup with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))

# simulate independent X, Y and astandardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
 y <- rnorm(100,84,1)
x[-23] <- scale(x[-23]) 
y[-23] <- scale(y[-23])

# plot shows outlier
qplot(x,y, alpha = 0.5)
# outliet makes it appear there is a correlation
cor(x,y)
cor(x[-23], y[-23])

# use rank instead
qplot(rank(x), rank(y))
?rank

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y , method = "spearman")

#CAUSE AND EFFECT REVERSE
# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))
