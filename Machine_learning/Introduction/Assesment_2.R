# Assesmenet 2: intoduction to machine learning: iris
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
y

#create a test and training set
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#test which feature is better to detect the iris
iris
#test sepal length
cutoff_sep_len <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), 0.1)
accuracy_sep_len <- map_dbl(cutoff_sep_len, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
qplot(cutoff_sep_len,accuracy_sep_len)
#tesst sepal width
cutoff_sep_width <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), 0.1)
accuracy_sep_width <- map_dbl(cutoff_sep_width, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
qplot(cutoff_sep_width,accuracy_sep_width)
#tesst petal width
cutoff_pet_width <- seq(min(iris$Petal.Width), max(iris$Petal.Width), 0.1)
accuracy_pet_width <- map_dbl(cutoff_pet_width, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
qplot(cutoff_pet_width,accuracy_pet_width)

#test petal length
cutoff_pet_length <- seq(min(iris$Petal.Length), max(iris$Petal.Length), 0.1)
accuracy_pet_length <- map_dbl(cutoff_pet_length, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
qplot(cutoff_pet_length,accuracy_pet_length)
max(accuracy_pet_length)
max(accuracy_pet_width)
train
# alternative way test which feature is best to test
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	


cutoff_pet_length[which.max(accuracy_pet_length)]
y_hat <- ifelse(test$Petal.Length>4.7, "virginica", "versicolor") %>%
  factor(levels = levels(y))
mean(y_hat == test$Species)

# see which feature optimizes the test set
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==iris$Species)
  })
}
predictions <- apply(iris[,-5],2,foo)
sapply(predictions,max)

?apply

#exploratory data analysis
plot(iris, pch=21,bg=iris$Species)
?plot

#use tow cutoffs, define which feature of petal length maximizes the acccuracy of prediction
max_acc_pet_len <- cutoff_pet_length[which.max(accuracy_pet_length)]
max_acc_pet_width <- cutoff_pet_width[which.max(accuracy_pet_width)]
max_acc_pet_len
max_acc_pet_width
y_hat_pet_len_pet_width <- ifelse(test$Petal.Length > max_acc_pet_len | test$Petal.Width > max_acc_pet_width, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat_pet_len_pet_width == train$Species)
y_hat_pet_len_pet_width

