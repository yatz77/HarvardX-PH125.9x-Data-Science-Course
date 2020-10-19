library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
head(polls)
class(polls)
ncol(polls)

polls_2 <- polls %>%
  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")) %>%
  filter(grepl("%", remain))
head(polls_2)

names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls_3 <- polls[str_detect(polls$remain, "%"), -9]
head(polls_3)

str_replace(polls$undecided, "N/A", "0")

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
temp
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
