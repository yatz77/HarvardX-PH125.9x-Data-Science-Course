install.packages("gtools")
library(gtools)
library(tidyverse)
options(digits = 3)

# exercise 1 olympic runners
sample(8,3)
possibilities <- nrow(permutations(8,3)) # number of ways three medals can be distributed over 8 runners
jamaican <- nrow(permutations(3,3)) # number of ways three medals can be distributed over 3 Jamaican runners
jamaican/possibilities # probability that jamaican win all three medals

#Monte Carlo simulation Jamaican runner
B <- 10000
set.seed(1)

f <- sample(runners,3)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
mont <- replicate(B,{
  x <- sample(runners,3)
  all(x == "Jamaica")
})
mean(mont)

#exercise 2 restaurant management
entree <- c(1:6)
sides <- c(1:6)
drinks <- c(1:2)
possibilities <- expand.grid(entree,sides,sides,drinks)
nrow(possibilities)
6*nrow(combinations(6,3))*3

help(function)
entree <- seq(1,12)
meal_combinations <- function(n){
  n*6*5*3
}
  
#min entree choices to offer each day a new combination

entree_choices <- function(x){
  x * nrow(combinations(6,2)) * 3
}

combos <- sapply(1:12, entree_choices)

data.frame(entrees = 1:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)

side_choices <- function(x){
  6*nrow(combinations(x,2))*3
}

combos <- sapply(2:12, side_choices)

data.frame(side_choices = 2:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$combos)
}

#exercie 3 espogeal cancer and tobacco (part 1)
help("select")
head(esoph)
nrow(esoph)j # number of studied groups
all_cases <- sum(esoph$ncases) # all cancer cases
all_controls <- esoph %>% select(ncontrols) %>% sum() # all controls
levels(esoph$alcgp)

# chance that person with highest alcohol consumption is cancer case
cases_high_alcohol <- esoph %>% filter(alcgp == "120+") %>% select(ncases) %>% sum()
control_high_alcohol <- esoph %>% filter(alcgp == "120+") %>% select(ncontrols) %>% sum()
cases_high_alcohol/(cases_high_alcohol + control_high_alcohol)

# chance that person with lowest alcohol consumption is cancer case

    
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
  
# chance that a case smoke 10g or more a day
levels(esoph$tobgp)
1- esoph %>%
  filter(tobgp == "0-9g/day") %>%
  select(ncases) %>% sum()/all_cases

#chance control smoke 10g or more a day

cont_tob <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()
cont_tob/all_controls

#esophageal cancer and tobacco part 2
# chance a case is in the highest alcohol group
cases_alc <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
p_case_high_alc <- cases_alc/all_cases
levels(esoph$tobgp)

#chance a case is in the highest tobacco group
cases_tob <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
p_case_high_tob <- cases_tob/all_cases

# chance a case is in the highest tobacco and alcohol group

cases_tob_alc <- esoph %>%
  filter(tobgp == "30+", alcgp == "120+") %>%
  pull(ncases) %>%
  sum()
p_case_high_tob_alc <- cases_tob_alc/all_cases

# chance a case is in the highes tobacco or alcohol group

p_case_alc_or_tob <- p_case_high_alc + p_case_high_tob - p_case_high_tob_alc

# chance a control is in the highest alcohol group
control_high_alc <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
p_control_alc <- control_high_alc/all_controls

# how much more likely is a case to be in the highest alcohol group compared to control
p_case_high_alc/p_control_alc

#chance a control is in the highest tobacoo group
control_high_tob <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()
p_control_tob <- control_high_tob/all_controls
 
#chance a control is in the highest tobacco and alcohol group
control_high_tob_alc <- esoph %>%
  filter(tobgp == "30+", alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()
p_control_tob_alc <- control_high_tob_alc/all_controls

# chance a control is in the highest tobacco or alcohol group
p_control_alc_or_tob <- p_control_alc + p_control_tob - p_control_tob_alc

# how many times a case is in the highest alchol group or tobacco group
p_case_alc_or_tob/p_control_alc_or_tob
