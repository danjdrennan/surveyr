source("R/srs_functions.R")
# Goal is to sort out the implementation for sampling_distribution using variable
# SRS statistics. Need to generate a few datasets and check how arguments work in
# the apply function.

# First make data for a mean
n <- 200
p <- 5
X <- matrix(rnorm(n*p), n, p)

# Want to confirm results with and without weights
N <- 1027
weights <- rep(N/n, p)
apply(X, 1, .mean)
apply(X, 1, .mean, weights=weights)

# Now check totals
apply(X, 1, .total, N=N)
apply(X, 1, .total, N=N, weights=weights)

# Now proportions (generate 0-1 variables)
X <- ifelse(X < mean(X), 0, 1)
apply(X, 1, .prop)
apply(X, 1, .prop, weights=weights)
