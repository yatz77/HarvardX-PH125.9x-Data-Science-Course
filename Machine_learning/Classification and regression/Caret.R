library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y ~ ., methhod = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = T)

train_knn <- train(y ~ .,  method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = T)
train_knn$bestTune
train_knn$finalModel
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = T)

train_knn$results %>%
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k,
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p = p_hat)
  }
  tmp%>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = F) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks = c(0.5), color="black")
}
plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

predict(train_knn, mnist_27$true_p, type = "prob")[,2]
mnist_27$true_p

install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
grid

train_loess <- train(y ~ .,
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = T)

confusionMatrix(data = predict(train_loess, mnist_27$test),
                 reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

#ASSESMENT
library(rpart)
data("tissue_gene_expression")
library(caret)
?train
tissue_tree <- rpart(tissue_gene_expression$y ~ tissue_gene_expression$x)
plot(tissue_tree, margin = 0.1)
text(tissue_tree, cex = 0.75)

# tune cp for the rpart method
set.seed(1991, sample.kind = "Rounding")
fit_rpart <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit_rpart)

#now allow rpart to split without restrictions to the minimum number of observations before splitting a node

set.seed(1991, sample.kind = "Rounding")
fit <- with(tissue_gene_expression,
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                  control = rpart.control(minsplit = 0)))

# best tree
best_tree <- rpart(tissue_gene_expression$y ~ tissue_gene_expression$x, minsplit = 0, cp = 0)
plot(best_tree, margin = 0.1)
text(best_tree, cex = 0.75)

# see if even fewer genes allow us to predict the tissue type with random forest
set.seed(1991, sample.kind = "Rounding")
fit_rf <- with(tissue_gene_expression,
               train(x, y, method = "rf",
                     tuneGrid = data.frame(mtry = seq(50, 200, 25)),
                     nodesize = 1))

# variable importance
varImp(fit_rf)
# extract predictor names rpart
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))

# extract predictor names rt
tree_terms <- as.character(unique(fit_rf$finalModel$
tree_terms
