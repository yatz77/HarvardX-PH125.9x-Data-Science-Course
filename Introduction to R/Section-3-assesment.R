library(dslabs)
library(dplyr)
data(heights)
options(digits = 3)
heights

mean_height <- mean(heights$height)
mean_height
help(filter)
ind <- heights$height > mean_height
class(ind)
ind
sum(ind)
female <- heights$sex == "Female"
mean(female)
minimum_height <- min(heights$height)
minimum_height
match(minimum_height,heights$height)
heights[1032]
help(filter)
heights$sex[1032]

maximum_height <- max(heights$height)
maximum_height
x <- minimum_height:maximum_height
class(x)
x
y <- !x %in% heights$height
y
sum(y)

heights2 <- mutate(heights, ht_cm = heights$height*2.54)
heights2$ht_cm[18]
mean(heights2$ht_cm)
female <- heights2$sex == "Female"
sum(female)
mean(heights2$ht_cm[female])

data(olive)
head(olive)
plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region,olive)
