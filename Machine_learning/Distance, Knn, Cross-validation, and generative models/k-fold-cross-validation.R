# COMPREHENSION CHECK 

set.seed(1996, sample.kind = "Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n , p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

# now see which predictors are most predictive of the outcome using t tests for all predictors
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals < 0.01)
length(ind)
ind
#recalculate the accuracy of the logistic fitting model using the predictors which are most predictive
x_subset <- x[,ind]
x_subset
fit <- train(x_subset,y , method = "glm")
fit$results

?train
# now fit using knn
k = seq(101, 301, 25)

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# now use the train functinn on the tissue_gene_expression dataset
data("tissue_gene_expression")
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)
