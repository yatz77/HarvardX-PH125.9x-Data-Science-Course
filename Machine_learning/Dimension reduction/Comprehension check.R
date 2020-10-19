library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#visualize the first two pcas
pca <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(x = pc_1, y = pc_2, color = tissue))+
  geom_point()

class(tissue_gene_expression$x)
#look for biases due to experimental procedure or instrument
avg_all_pred <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pca$x[,1], avg = avg_all_pred, tissue = tissue_gene_expression$y) %>%
  ggplot(aes(x = pc_1, y = avg_all_pred, color = tissue))+
  geom_point()
cor(avg_all_pred,pca$x[,1])

# remove the center and redo the pca
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# make a boxplot of the first 10 PCs showing the vlaues for each tissue
install.packages("gridExtra")
library(gridExtra)
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

summary(pc)$importance[,1:10]
plot(summary(pc)$importance[3,])
