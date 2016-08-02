# generate binomial draws using hierarchical 3-mixture beta
set.seed(42)
n <- 100

# latent classes
k <- 3
p <- c(0.5, 0.15, 0.35)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)

# means and variances
mu <- c(-3, 6, 17)

# generate theta
X <- rnorm(n, mu[z])

# save data to use for models
saveRDS(X, "data/binomial.RDS")
