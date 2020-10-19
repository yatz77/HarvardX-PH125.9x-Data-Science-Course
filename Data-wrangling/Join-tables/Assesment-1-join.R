install.packages("Lahman")
library(Lahman)

top <- Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>% # arrange by descending HR count
  slice(1:10) # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top  %>% left_join(Master) %>% select(playerID, nameFirst, nameLast, HR)
head(Salaries)
head(top_names)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
head(top_salary)
head(AwardsPlayers)

Awards_2016 <- AwardsPlayers %>% filter(yearID == "2016")
Awards_2016
intersect(Awards_2016$playerID,top_names$playerID)
awards_non_top_10 <- setdiff(Awards_2016$playerID, top_names$playerID)
class(awards_non_top_10)
length(awards_non_top_10)
