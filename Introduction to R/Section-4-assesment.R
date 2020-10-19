library(dslabs)
data(heights)

result <- ifelse(heights$sex == "Female", 1, 2)
sum(result)

result2 <- ifelse(heights$height > 72, heights$height, 0)
mean(result2)

inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)

result3 <- ifelse(inches_to_ft(heights$height) < 5, 1, 0)
result3
sum(result3)


# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for (n in 1:m){
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n
