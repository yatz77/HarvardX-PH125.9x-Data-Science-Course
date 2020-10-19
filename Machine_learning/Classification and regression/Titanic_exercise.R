library(dslabs)
library(titanic)
install.packages("titanic")
library(titanic)
library(caret)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - 'titanic_train' is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = T), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>% # count family members
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# split titanic_clean into train and test sets
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = F)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)
# number of observations in train and test set
nrow(train_set)
nrow(test_set)
train_set
mean(as.numeric(as.character(train_set$Survived)))

#question 2: guessing who survives

set.seed(3, sample.kind = "Rounding")
random_survived <- sample(c(0,1), size = 179, replace = T)
random_survived
confusionMatrix(as.factor(random_survived), test_set$Survived)$overall["Accuracy"]
mean(as.factor(random_survived) == test_set$Survived)

# Q3 predict survival from sex
train_set %>% filter(Sex == "male") %>% summarize(mean(Survived == 1))
test_set %>% mutate(y_hat = ifelse(Sex == "female", 1, 0)) %>% summarize(mean(Survived == y_hat))

# Q4 survival by passenger class
titanic_clean %>% filter(Pclass == 2) %>% summarize(mean(Survived == 1))
test_set %>% mutate(y_hat = ifelse(Pclass == 1, 1, 0)) %>% summarize(mean(Survived == y_hat))
titanic_clean %>% group_by(Sex, Pclass) %>% summarize(mean(Survived == 1))
#use both sex and passenger class to determine survival rate
test_set %>% mutate(y_hat = ifelse(Sex == "female" & Pclass %in% c(1,2), 1, 0)) %>% summarize(mean(Survived == y_hat))

#Q5 create confusion matrices for all models
#only sex
sex_model <- test_set %>% mutate(y_hat = ifelse(Sex == "female", 1, 0)) %>% mutate(y_hat = as.factor(y_hat), Survived = as.factor(Survived)) 
confusionMatrix(data = sex_model$y_hat, reference = sex_model$Survived)
#only class
class_model <- test_set %>% mutate(y_hat = ifelse(Pclass == 1, 1, 0)) %>% mutate(y_hat = as.factor(y_hat), Survived = as.factor(Survived)) 
confusionMatrix(data = class_model$y_hat, reference = class_model$Survived)
# class and sex
class_sex_model <- test_set %>% 
  mutate(y_hat = ifelse(Sex == "female" & Pclass %in% c(1,2), 1, 0)) %>%
  mutate(y_hat = as.factor(y_hat), Survived = as.factor(Survived))
confusionMatrix(data = class_sex_model$y_hat, reference = class_sex_model$Survived)

# 6 F scores
# sex model
F_meas(data = sex_model$y_hat, reference = sex_model$Survived)
# class model
F_meas(data = class_model$y_hat, reference = class_model$Survived)
# sex and class model
F_meas(data = class_sex_model$y_hat, reference = class_sex_model$Survived)


#Q7 survival by fare- LDA and QDA
set.seed(1, sample.kind = "Rounding")
train_fare_LDA <- train(Survived ~ Fare, method = "lda", data = train_set)
confusionMatrix(data = predict(train_fare_LDA, test_set), reference = test_set$Survived)
set.seed(1, sample.kind = "Rounding")
train_fare_QDA <- train(Survived ~ Fare, method = "qda", data = train_set)
confusionMatrix(data = predict(train_fare_QDA, test_set), reference = test_set$Survived)

#Q8 train logistic regression 
#using age as predictor
set.seed(1, sample.kind = "Rounding")
train_age_glm <- train(Survived ~ Age, method = "glm", data = train_set)
confusionMatrix(data = predict(train_age_glm, test_set), reference = test_set$Survived)
#using age, sex, class and fare as predictors
set.seed(1, sample.kind = "Rounding")
train_scfa_glm <- train(Survived ~ Age + Sex + Pclass + Fare, method = "glm", data = train_set)
confusionMatrix(data = predict(train_scfa_glm, test_set), reference = test_set$Survived)
# using all predictors
set.seed(1, sample.kind = "Rounding")
train_all_glm <- train(Survived ~ ., method = "glm", data = train_set)
confusionMatrix(data = predict(train_all_glm, test_set), reference = test_set$Survived)

#Q9 using knn
?train
set.seed(6, sample.kind = "Rounding")
train_all_knn <- train(Survived ~ ., method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)), data = train_set)
plot(train_all_knn)
train_all_knn$bestTune
confusionMatrix(data = predict(train_all_knn, test_set), reference = test_set$Survived)

#Q10 knn with 10-fold cross validation
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .1)
train_all_knn_cv <- train(Survived ~ ., method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)), 
                          trControl = control, data = train_set)
plot(train_all_knn_cv)
train_all_knn_cv$bestTune
confusionMatrix(data = predict(train_all_knn_cv, test_set), reference = test_set$Survived)

# Q11 classification tree model
set.seed(10, sample.kind = "Rounding")
train_all_rpart <- train(Survived ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), data = train_set)
plot(train_all_rpart)
train_all_rpart$bestTune
confusionMatrix(data = predict(train_all_rpart, test_set), reference = test_set$Survived)
train_all_rpart$finalModel
plot(train_all_rpart$finalModel)
text(train_all_rpart$finalModel)

#Q12 random forest model
set.seed(14, sample.kind = "Rounding")
train_all_rf <- train(Survived ~ ., method = "rf", tuneGrid = data.frame(mtry = seq(1, 7, 1)), ntree = 100, data = train_set)
plot(train_all_rf)
confusionMatrix(data = predict(train_all_rf, test_set), reference = test_set$Survived)
varImp(train_all_rf)
