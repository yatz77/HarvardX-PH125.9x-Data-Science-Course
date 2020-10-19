library(dslabs)
library(lubridate)
options(digits = 3) # 3 significant digits
data("brexit_polls")
head(brexit_polls)


brexit_polls %>%
  filter(month(ymd(startdate)) == 4)%>%
  nrow()

sum(week(round_date(brexit_polls$enddate, unit="week")) == week(ymd("2016-06-12")))

brexit_polls %>% 
  mutate(weekdays = weekdays(enddate)) %>%
  count(weekdays)
  
data(movielens)

head(movielens)

movielens %>%
  mutate(date = as_datetime(timestamp), hour_review = hour(date)) %>%
  count(hour_review) %>%
  arrange(desc(n))

#Assesment 2

library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
library(tidytext)
options(digits = 3)
library(stringr)

gutenberg_metadata
sum(str_detect(gutenberg_metadata$title,"Pride\\s?and\\s?Prejudice"), na.rm = T)
?str_detect

gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

gutenberg_metadata %>%
  gutenberg_works() %>%
  filter(str_detect(title, "Pride and Prejudice"))
gutenberg_works()%>%
  filter(str_detect(title, "Pride and Prejudice"))

library(tidytext)

PP <- gutenberg_download(1342)
?tidytext
PP

words <-   PP %>%
  extract(text,"words", regex = "([a-zA-Z]+)")
words
PP %>% str_split(text,"\\s?", simplify = T)
words <- PP %>% unnest_tokens(word,text)
nrow(words)



No_stop_words <- words %>% 
  filter(!word %in% stop_words$word)
nrow(No_stop_words)

No_stop_words_no_digits <- No_stop_words %>%
  filter(!str_detect(word,"[1-9]"))
No_stop_words_no_digits

No_stop_words_no_digits %>%
  count(word) %>% 
  filter(n > 100) %>%
  arrange(desc(n))

afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(No_stop_words_no_digits,afinn)
nrow(afinn_sentiments)
x <- afinn_sentiments %>%
  filter(value >= +1) %>%
  nrow()
x
y <- afinn_sentiments %>%
  nrow()
y
x/y
afinn_sentiments%>%
  count(word)
