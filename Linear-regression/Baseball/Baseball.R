library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
options(digits = 3)

# Relationshio between homeruns and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# Relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# Relatioinship betweeen stolen bases and runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R/ G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# Relatioinship betweeen fielding errors and wins

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(E_per_game = E / G, W_per_game = W/ G) %>%
  ggplot(aes(E_per_game, W_per_game)) +
  geom_point(alpha = 0.5)

# Relationship betweeen triples per game and doubles per game

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B/ G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) +
  geom_point(alpha = 0.5)

# correlation coefficient between number of runs per game and number of at bats per game

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R /G) %>%
  summarize(r = cor(R_per_game,AB_per_game)) %>% pull(r)

# correlation coefficient between number of wins per game and number of errors per game

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(E_per_game = E / G, W_per_game = W /G) %>%
  summarize(r = cor(W_per_game,E_per_game)) %>% pull(r)

# correlation coefficient between doubles per game and triples per game

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B / G, X3B_per_game = X3B /G) %>%
  summarize(r = cor(X3B_per_game,X2B_per_game)) %>% pull(r)

#INTRODUCTION TO LINEAR MODELS

# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  lm(R_per_game ~ BB_per_game, data = .) %>%
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

# STRATIFICATION AND MULTIVARIATE ANALYSIS

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

# scatterplot for each HR stratum
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game) * sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game) *sd(R_per_game)/sd(HR_per_game))

#ASSESMENT!: LSE
#linear model predicting number of runs per game based on bases on balls and number of home runs
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G,
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  lm(R_per_game ~ BB_per_game +  HR_per_game, data = .)
dat
  
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

# Assesment 2 least squares estimates

# stability singles and BB
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR) /pa, bb = BB/pa) %>%
  filter(pa >= 100) 

bat_03 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >=100) 

bat_03 %>%
  nrow()

mean_singles <- bat_03%>%
  group_by(playerID) %>%
  arrange(playerID) %>%
  mutate(mean_singles = mean(singles)) %>%
  filter(mean_singles>0.2) %>%
  select(playerID,mean_singles)%>%
  distinct() %>%
  nrow()
mean_singles


mean_BB <- bat_03%>%
  group_by(playerID) %>%
  arrange(playerID) %>%
  mutate(mean_BB = mean(bb)) %>%
  filter(mean_BB>0.2) %>%
  select(playerID,mean_BB)%>%
  distinct() %>%
  nrow()
mean_BB

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
bat_99_01

#correlation between 2002 single rates and mean single rates 1999-2001
bat02_and_bat_99_01 <- inner_join(bat_02,bat_99_01)
bat02_and_bat_99_01 %>% summarize(cor = cor(singles, mean_singles)) %>% pull(cor)

#correlation between 2002 BB rates and mean BB rates 1999-2001
bat02_and_bat_99_01 %>% summarize(cor = cor(bb, mean_bb)) %>% pull(cor)

# scatterplot mean singles versus 2002 singles
bat02_and_bat_99_01 %>% ggplot(aes(singles, mean_singles)) +
  geom_point()

# scatterplot mean bb versus 2002 bb
bat02_and_bat_99_01 %>% ggplot(aes(bb, mean_bb)) +
  geom_point()

#predict 2002 singles from 1999-2001 singles
lm_singles <- bat02_and_bat_99_01 %>% lm(singles ~ mean_singles, data = .)
  predictions <- predict(lm_singles)
data <- as.tibble(predictions)
data
lm_singles

#predict 2002 bb from 1999-2001 bb
lm_singles <- bat02_and_bat_99_01 %>% lm(bb ~ mean_bb, data = .)
predictions <- predict(lm_singles)
data <- as.tibble(predictions)
data
lm_singles

#ADVANCED DPLYR:TIBBLES

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G,1),
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR <=1.2)
dat
# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

# TIBBLES DIFFERENCES FROM DATA FRAMES

# inspect data frame and tibble
Teams
as.tibble(Teams)

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as.tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as.tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as.tibble(Teams)$hr

# create a tibble with complex objects
tibble(id = c(1,2,3), func = c(mean, median, sd))

#TIBBLES AND DO
# use do to fit a regression line to each HR stratum
dat %>%
  group_by(HR) %>%
  do(fit= lm(R~ BB, data = .))

# using do without a column name gives anerror
dat %>%
  group_by(HR) %>%
  do(lm(R~BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R~ BB, data = data)
  data.frame(term= names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[,2])
}

dat%>%
  group_by(HR) %>%
  do(get_lse(.))

# USE TIDY TO RETURN LM ESTIMATES AND RELATED INFORMATION AS A DATA FRAME
library(broom)
fit <- lm(R~ BB, data = dat)
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int = T)

# pipeline with lm, do, tidy
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R~BB, data = .), conf.int = T)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# make ggplots 
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = T)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# inspect eith glance
glance(fit)

# linear regression with two variables

fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB / G,
         singles = (H - X2B - X3B - HR) /G,
         doubles = X2B / G,
         triples = X3B /G,
         HR = HR / G,
         R = R /G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = T)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x = 0.1, cex = 2) +
  geom_abline()

# average number of tema plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  pull(pa_per_game) %>%
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >=300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))
players
pa_per_game

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players<- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")
players

# add defensive position
position_names <- c("G_p", "G_c", "G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>%
  filter(yearID == 2002) %>%
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max)
Appearances
tmp_tab
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))
players

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

#top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)
# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, col = POS)) +
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#bUILDING A BETTER OFFENSIVE METRIC: LINEAR PROGRAMMING
install.packages("reshape2", "lpSolve")
library(reshape2)
library(lpSolve)
library(tidyverse)
library(dslabs)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
constraint_matrix
npos <- nrow(constraint_matrix)
npos
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_dir
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

#REGRESSION FALLACY
# create table with player ID, their names and their most played position
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1)  %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)
help("slice")
playerInfo

# table woth only ROY award winners and batting statistics
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

#only rookie and sophomore seasons and remove players who did not play sophomre seasons
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# code for spread function to have one column for rookie and sophomore years batting averages
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY


#proportion player having lower batting average in sophomore year
mean(ROY$sophomore - ROY$rookie <= 0)

#similar analysis on all players in season 2013 and 2014 and batted more than 130 times
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

#worst performers 2013
arrange(two_years, `2013`)

#correlation performance two years
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`, `2014`))

#ASSESMENT 2: REGRESSION AND BASEBALL
Teams

#BB and HR effect on runs for 1971
BB_HR_effect <- Teams %>% filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .)
tidy(BB_HR_effect, conf.int = T)

#BB and HR effect on runs between 1961 to 2018
BB_HR_effect <- Teams %>% 
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID)%>%
  do(tidy(lm(R ~ BB + HR, data = .)))%>%
  filter(term %in% c("BB","HR"))
BB_HR_effect

# see effect of year on BB
BB_HR_effect %>% 
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate))
  
BB_year_effect <- BB_HR_effect %>% 
  ungroup() %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .)
tidy(BB_year_effect, conf.int = T)

#ASSESMENT LINEAR MODELS

#calculate average attendance baseball
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(avg_attendance = attendance/G)

# predict average attendance from runs per game
as.tibble(Teams_small)
attendance_runs <- Teams_small %>%
  mutate(run_per_game = HR/G) %>%
  lm(avg_attendance ~ run_per_game, data = .)
tidy(attendance_runs, conf.int = T)

# predict average attendance from homeruns per game
as.tibble(Teams_small)
attendance_homeruns <- Teams_small %>%
  mutate(homerun_per_game = HR/G) %>%
  lm(avg_attendance ~ homerun_per_game, data = .)
tidy(attendance_homeruns, conf.int = T)

# predict average attendance from wins in a season
attendance_wins <- Teams_small %>%
  lm(avg_attendance ~ W, data = .)
tidy(attendance_wins, conf.int = T)

# predict average attendance from years
attendance_year <- Teams_small %>%
  lm(avg_attendance ~ yearID, data = .)
tidy(attendance_year, conf.int = T)

# correlation of wins with runs per game
cor_runs_wins <- Teams_small %>%
  mutate(run_per_game = R/G) %>%
  summarize(cor = cor(W, run_per_game))%>%
  pull(cor)
cor_runs_wins

# correlation of wins with homeruns per game
cor_homeruns_wins <- Teams_small %>%
  mutate(homerun_per_game = HR/G) %>%
  summarize(cor = cor(W, homerun_per_game))%>%
  pull(cor)
cor_homeruns_wins

#stratify according to wins
Teams_small_wins <- Teams_small %>%
  mutate(round_wins = round(W/10), run_per_game = R/G, homerun_per_game = HR/G) %>%
  filter(round_wins %in% 5:10) %>%
  group_by(round_wins)%>%
  filter(n()>20)

# how many observations in the 8 won strata
Teams_small_wins %>% filter(round_wins == 8) %>% nrow()

# slope regression line predicting average attendance given runs per game
Teams_small_wins %>% do(tidy(lm(avg_attendance ~ run_per_game, data = .), conf.int = T)) %>%
  filter(term == "run_per_game")

# slope regression line predicting average attendance given runs per game
Teams_small_wins %>% do(tidy(lm(avg_attendance ~ homerun_per_game, data = .), conf.int = T)) %>%
  filter(term == "homerun_per_game")

# multivariate regression determining effects of runs per game, homeruns per game, wins and year on average attendance
multi_attendance <- Teams_small %>%
  mutate(run_per_game = R/G, homerun_per_game = HR/G) %>%
  lm(avg_attendance ~ run_per_game + homerun_per_game + W + yearID, data = .)
tidy(multi_attendance, conf.int = T)

predict(multi_attendance, data.frame(run_per_game = 5, homerun_per_game = 1.2, W = 80, yearID = 1960))

# correlate actual attendance with predicted attendance
cor_predictedatt_actualatt <- Teams%>%
  filter(yearID == 2002)%>%
  mutate(run_per_game = R/G, homerun_per_game = HR/G,
         avg_attendance = attendance/G) %>%
  mutate(avg_attendance_hat = predict(multi_attendance, newdata = .))%>%
  summarize(cor = cor(avg_attendance,avg_attendance_hat))%>%
  pull(cor)
cor_predictedatt_actualatt
%>% 