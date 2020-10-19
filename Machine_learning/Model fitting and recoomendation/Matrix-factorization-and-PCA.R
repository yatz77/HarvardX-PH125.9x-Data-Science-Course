library(matrixStats)
train_small <- movielens %>%
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woan used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()
y
rownames(y) <- y[,1]
y <- y[,-1]
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
colnames(y)

y <- sweep(y, 1, rowmeans(y, na.rm=T))
y <- sweep(y, 2, colMeans(y, na.rm=T))

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

cor(y[, c(m_1,m_2,m_3,m_4,m_5)], use="pairwise.complete") %>%
  knitr::kable()

set.seed(1, sample.kind = "Rounding")
options(digits = 2)
Q <- matrix(c(1, 1, 1, -1, -1), ncol = 1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol = 1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(align = "c")
P

set.seed(1, sample.kind = "Rounding")
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1, 1, 1, -1, -1, -1),
           c(1, 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
P
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(align="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

#SVD and PCA
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)
y
dim(pca$rotation)
dim(y)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

install.packages("ggrepel")
library(ggrepel)

pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>% ggplot(aes(PC1, PC2)) + geom_point() +
  geom_text_repel(aes(PC1, PC2, label = name),
                  data = filter(pcs,
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)
pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

# COMPREHENSION CHECK 1: SVD
set.seed(1987, sample.kind = "Rounding")
n <- 100
k <- 8
Sigma <- 64 * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3)
m <- MASS::mvrnorm(n, rep(0,3), Sigma)
m <- m[order(rowMeans(m), decreasing=T),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
matrix(rnorm(matrix(n*k*3)), n, k*3)

colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
install.packages("RColorBrewer")
library(RColorBrewer)
my_image <- function(x, zlim = range(x),...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=F]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows+0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# cpmpute sum of squares for columns of Y and for YV
ss_y <- colSums(y^2)
yv <- y %*% s$v
ss_yv <- colSums(yv^2)

sum(ss_y)
sum(ss_yv)

# plot y and yv against column number

library(tidyverse)
library(dslabs)

qplot(x=c(1:24), y = ss_y)
qplot(x=c(1:24), y = ss_yv)
qplot(x=s$d, y = sqrt(ss_yv))

library(matrixStats)
var_yv <- colVars(yv)
(var_yv[1] + var_yv[2] + var_yv[3])/sum(var_yv)

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

# averages score should explain a lot of the variability
avg_rows <- rowMeans(y)
ud <- sweep(s$u, 2, s$d, FUN = "*")
qplot(ud[,1], avg_rows)

# make an image plot of V
s$v
image(s$v, 1:nrow(s$v), 1:ncol(s$v))
my_image(s$v)

# make an image of UDVt and comere to image of y

UdV_one <- s$u[,1,drop=F]%*%diag(s$d)[1,1,drop=F]%*%t(s$v[,1,drop=F])
my_image(UdV_one)
my_image(y)

plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))

# look at residuals after removing the good student effect

resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# now image UdVt_2
UdV_two <- s$u[,2,drop=F]%*%diag(s$d)[2,2,drop=F]%*%t(s$v[,2,drop=F])
my_image(UdV_two)
my_image(resid)

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

# new residuals after adjusting for average student grade and differneces between sci/math and arts

resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# now imaging UdVt_3
UdV_three <- s$u[,3,drop=F]%*%diag(s$d)[3,3,drop=F]%*%t(s$v[,3,drop=F])
my_image(UdV_three)
my_image(resid)

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

resid <- y - with(s, sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# image y, image of d1U1V1t+d2U2V2t+d3U3V3t and residuals
my_image(UdV_one + UdV_two + UdV_three)
my_image(y)
my_image(resid)

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))

##CLUSTERING

# heat map
data("tissue_gene_expression")
x <- sweep(tissue_gene_expression$x, 2, colMeans(tissue_gene_expression$x))
h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))
dist(x)
image(x[h_1$order, h_2$order])
h_1$order
heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"))

library(matrixStats)
sds <- colSds(x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:25]
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))
#COMPREHENSION CHECK. CLUSTERING
data("tissue_gene_expression")

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

plot(hclust(d), labels = tissue_gene_expression$y, hang = 0.1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = NULL, ylab = "Height")

?hclust

# k-means clustering of tissue data
k <- kmeans(d, 7)
cluster_liver <- data.frame(cluster = k$cluster, tissue = tissue_gene_expression$y)
cluster_liver %>% filter(tissue == "liver")

# select the 50 most variable genes
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = T)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), ColSideColors = colors)
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
