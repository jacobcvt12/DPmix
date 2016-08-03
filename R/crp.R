library(R2jags)
library(shinystan)

X <- readRDS("data/normal.RDS")
n <- length(X)
m <- numeric(n)

# mixture of beta binomial model
model <- function() {
    # atom likelihoods
    for (i in 1:n) {
        X[i] ~ dnorm(theta[i], 0.1)
        theta[i] <- phi[table[i]]
    }
    
    # 2 to nth customer
    for (i in 2:n) {
        # construct probability of seats
        prob <- m
        prob[(sum(m) > 0) + 1] <- alpha
        
        # choose seat
        table[i] ~ dcat(prob[])
        
        # increase people at chosen table
        m[table[i]] <- m[table[i]] + 1
    }
    
    # first customer sits at first table
    prob[i] <- 1
    
    # baseline distribution
    for (k in 1:N) {
        phi[k] ~ dnorm(0, 0.0001)   
    }
    
    # DPP parameter prior
    alpha ~ dunif(0.3, 10)
}

model.data <- c("X", "n", "m")
model.params <- c("alpha", "theta", "table", "m")

# fit model
set.seed(42)
fit <- jags(model.data, NULL, model.params, model, 
            n.iter=30000)

# diagnose model
launch_shinystan(as.shinystan(as.mcmc(fit)))
