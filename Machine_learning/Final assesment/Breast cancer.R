options(digits = 3)
library(matrixStats)
library(dslabs)
library(tidyverse)
library(caret)
data("brca")

#Q1 dimensions and proportions
head(brca)
summary(brca)
as_tibble(brca$x)
mean(brca$y == "B")
which.min(colSds(brca$x))

#Q2Scaling the matrix
avg_cols <- colMeans(brca$x)
sds_cols <- colSds(brca$x)
cent_x <- sweep(brca$x,2, avg_cols, FUN = "-")
cent_x <- sweep(cent_x, 2, sds_cols, FUN = "/")
cent_x

colSds(cent_x)[2]
colMedians(cent_x)[1]

#Q3 distance
d <- dist(cent_x)
benign_indices <- which(brca$y == "B")
as.matrix(d)

# avg distance between first sample and other benign/malign samples
mean(as.matrix(d)[1,-benign_indices])

#Q4 heatmap of features
dist_features <- dist(t(cent_x))
heatmap(as.matrix(dist_features), labRow = NA, labCol = NA)

#Q5 hierarchical clustering
h <- hclust(dist_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

#Q6 PCA on scaled matrix
pca <- prcomp(cent_x)
var <- pca$sdev^2
var[1] / sum(var)
sum(cumsum(var)/max(cumsum(var)) <= 0.9)
cumsum(var)/30
pca$x
data.frame(PC_1 = pca$x[,1], PC_2 = pca$x[,2], assignment = brca$y) %>%
  ggplot(aes(PC_1, PC_2, col = assignment)) +
  geom_point()

#Q8 boxplot of first 10 PCs grouped by tumor type
boxplot(pca$x[,1:10])

data = data.frame(pca$x[,1:10])
data <- gather(data, key = components, value = values)
data %>% mutate(assignment = rep(brca$y,10)) %>%
  ggplot(aes(x = components, y = values, fill=assignment)) +
  geom_boxplot()



#Q9 

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = F)
test_x <- cent_x[test_index,]
test_y <- brca$y[test_index]
train_x <- cent_x[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y == "B")
mean(test_y == "B")
test_y

#Q10 K means clustering

predict_kmeans <- function(x, k) {
  centers <- k$centers # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances)) # select cluster with min distance to center
}
set.seed(3, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)
kmeans_preds <- as.factor(ifelse(predict_kmeans(test_x, k) == 1, "B", "M"))
kmeans_acc <- mean(kmeans_preds == test_y)

mean(kmeans_preds[which(kmeans_preds == "B")]==test_y[which(test_y == "B")])

test_benign_indices <- which(test_y == "B")
mean(kmeans_preds[test_benign_indices]==test_y[test_benign_indices])
test_malign_indices <- which(test_y == "M")
mean(kmeans_preds[test_malign_indices]==test_y[test_malign_indices])        

# Q11 logistic regression model fit
train_glm <- train(train_x, train_y,
                   method = "glm")
glm_preds <- predict(train_glm, test_x)
glm_acc <- mean(glm_preds == test_y)

# Q12 train an LDA and QDA model on the training set
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
lda_acc <- mean(lda_preds == test_y)

train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
qda_acc <- mean(qda_preds == test_y)

# Q13 fit a loess model

set.seed(5, sample.kind = "Rounding")
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
loess_acc <- mean(loess_preds == test_y)

# Q14 fit a k-nearest neighbour model

set.seed(7, sample.kind = "Rounding")
train_knn <- train(train_x, train_y, method = "knn", tuneGrid =data.frame(k= seq(3, 21, 2)))
knn_preds <- predict(train_knn, test_x)
knn_acc <- mean(knn_preds == test_y)

#Q 15 random forest model

set.seed(9, sample.kind = "Rounding")
train_rf <- train(train_x, train_y, method = "rf", tuneGrid =data.frame(mtry= seq(3, 9, 2)), importance = T)
rf_preds <- predict(train_rf, test_x)
rf_acc <- mean(rf_preds == test_y)
varImp(train_rf)

# Q16
ensemble <- ((kmeans_preds=="B") + (log_pred=="B") + (lda_preds=="B") + (qda_preds=="B") + (loess_preds=="B") + (knn_preds=="B") + (rf_preds=="B"))/7
pred_ensemble <- ifelse(ensemble > 0.5, "B", "M")
acc_ensemble <- mean(pred_ensemble == test_y)

tibble(acc_ensemble, loess_acc, knn_acc, qda_acc, lda_acc, glm_acc, rf_acc, kmeans_acc )
