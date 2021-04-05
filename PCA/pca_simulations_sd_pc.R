#  simulate the points
mean1 = 15
sd1 = 10
mean2 = 0
sd2 = 1

dims <- 90
obs <- 200
normal_points <- matrix(sample(c(rnorm(round(dims*obs*0.875), mean1, sd1),
                                 rnorm(round(dims*obs*0.125), mean2, sd2))), obs, dims)
normal_points <- matrix(rnorm(obs*dims, mean2, sd2), obs, dims)
qplot(normal_points[,1], normal_points[,2]) +
  geom_density_2d() +
  coord_equal()

#set.seed(44715971)
# now, let's make them correlated
A <-  diag(dims)
A[A == 0] <- runif(sum(A==0))
X <- normal_points %*% A
plt_normal <-
  qplot(X[,1], X[,2]) +
  geom_density_2d() +
  coord_equal()

# run PCA
pca_ <- prcomp(X, center = TRUE, scale. = TRUE)

center_mat <- matrix(rep(colMeans(X), each = nrow(X)), 
                     nrow = nrow(X)) 
transformed_data <- 
  (scale(X)) %*% pca_$rotation

plt_pca <-
 qplot(pca_$x[,1], # equivalent to: qplot(pca_$x[,1], pca_$x[,2])
       pca_$x[,2], 
       alpha = .4) + 
 labs(x = "PC1", y = "PC2") +
 geom_density2d(alpha = .4) +
 labs(title = "PCA projection") +
 #coord_equal() +
 theme(legend.position = "none")

plt_pca

var_expl <-
  pca_$sdev^2 / sum(pca_$sdev^2)
var_expl

dist_X <- dist(scale(X))
mds_ <- cmdscale(dist_X, k = 10)
mds_2 <- MASS::isoMDS(dist_X)
plt_mds <-
  qplot(mds_[,1], # equivalent to: qplot(pca_$x[,1], pca_$x[,2])
        mds_[,2], 
        alpha = .4) + 
  labs(x = "dim 1", y = "dim 2") +
  geom_density2d(alpha = .4) +
  labs(title = "MDS (cmdscale) projection") +
  #coord_equal() +
  theme(legend.position = "none")

plt_mds2 <-
  qplot(mds_2$points[,1], # equivalent to: qplot(pca_$x[,1], pca_$x[,2])
        mds_2$points[,2], 
        alpha = .4) + 
  labs(x = "dim 1", y = "dim 2") +
  geom_density2d(alpha = .4) +
  labs(title = "MASS::isoMDS projection") +
  #coord_equal() +
  theme(legend.position = "none")

layout <- "
###AAA###
###AAA###
BBBCCCDDD
"

plt_normal + plt_pca + plt_mds + plt_mds2 + plot_layout(design = layout)
 