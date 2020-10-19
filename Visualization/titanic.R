options(digits =3) # report 3 significant digits
library(tidyverse)
library(dslabs)
install.packages("titanic")
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
titanic_train
class(titanic_train$Sex)
class(titanic$Sex)
?titanic_train
head(titanic_train)

titanic %>% group_by(Sex) %>% ggplot(aes(Age, stat(count), fill = Sex)) + geom_density(alpha = 0.2)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) + geom_abline()

titanic %>% ggplot(aes(Survived, fill = Sex)) + geom_bar()

titanic %>% ggplot(aes(Survived, fill = Sex)) + geom_bar(position = position_dodge())

titanic %>% group_by(Survived) %>% ggplot(aes(Age, fill = Survived)) + geom_density(alpha = 0.2)

titanic %>% filter(Fare > 0)  %>% 
  ggplot(aes(x = Survived,y = Fare)) + 
  geom_boxplot(aes(group = Survived)) + 
  scale_y_continuous(trans = "log2") +
  geom_jitter(width = 0.1, alpha = 0.2)
  

titanic %>% 
  ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar()    

titanic %>% 
  ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar(position = position_fill()) 

titanic %>% 
  ggplot(aes(Survived, fill = Pclass)) + 
  geom_bar(position = position_fill()) 

titanic %>% group_by(Survived) %>% 
  ggplot(aes(Age, stat(count), fill = Survived)) + 
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ Pclass)
