#installing and loading the dplyr paxkage
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filster
filter(murders, rate<= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

#using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"),
                     exam_1 = c(95, 80, 90, 85),
                     exam_2 = c(90, 80, 85, 90),
                     stringsAsFactors = FALSE)
grades
class(grades)
