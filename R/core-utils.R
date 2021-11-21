## Contains several support functions that will be used throughout the package.
## The main contents are formulas for computing totals, means, and proportions
## under different survey designs (using stratification and clustering, namely).

# Functions for Simple Random Samples
# Start with the mean, then its variance and standard error and CV
.mean <- function(y, weights = NULL){
    if(is.null(weights)){
        return(mean(y))
    }else{
        # Must verify length of weights is same as length of y
        if(length(weights) != length(y)){
            stop("weights and y must be the same length")
        }
        return(sum(weights * y) / sum(weights))
    }
}

.var_mean <- function(y, N=NULL, fpc=TRUE){
    n <- length(y)
    # Only depend on N as input when computing finite population correction (fpc)
    if(fpc){
        if(is.null(N)){
            stop("N >= n must be supplied when using fpc")
        }else{
            if(N < n){
                stop("N < n provided. N >= n required.")
            }
        }
        s2 <- (1 - n / N) * var(y)
    } else{
        s2 <- var(y)
    }
    return(s2)
}

.se_mean <- function(y, N=NULL, fpc=TRUE){
    sqrt(.var_mean(y, N, fpc))
}

.cv_mean <- function(y, N=NULL, fpc=TRUE, weights=NULL){
    .se_mean(y, N, fpc) / .mean(y, weights)
}

# SRS Stats for a total
.total <- function(y, N=NULL, weights=NULL){
    if(is.null(weights) && is.null(N)){
        stop("N or weights must be supplied")
    }
    if(is.null(N)){
        N <- sum(weights)
    }else{
        if(length(N) > 1){
            stop("N must be an integer")
        }
    }
    return(N * .mean(y))
}

.var_total <- function(y, N, fpc=TRUE){
    N^2 * .var_mean(y, N, fpc)
}

.se_total <- function(y, N, fpc=TRUE){
    sqrt(.var_total(y, N, fpc))
}

.cv_total <- function(y, N=NULL, fpc=TRUE, weights=NULL){
    .cv_mean(y, N, fpc, weights)
}
