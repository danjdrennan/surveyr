## Core functions used to compute statistics for simple random samples.
## Functions for point estimates, variances, etc., are abstracted from the user
## and made available through the function mk_stats.

#' Estimate a statistic from a simple random sample design
#'
#' @description
#' Function to get point estimates and statistics associated with them.
#' Given a point estimate (mean, total, prop), `mk_stat` will generate the
#'   - sample size, n, at calling time
#'   - point estimate
#'   - estimator variance
#'   - standard error
#'   - coefficient of variation
#' assuming a simple random sample of data.
#'
#' @param y
#' A numeric vector with at least two elements
#' @param stat
#' A string, either "mean", "total", or "prop" (proportion)
#' @param N
#' The population size.
#' This must be supplied and must be larger than length(y).
#' If unknown, recommend passing it as some N > n with fpc=FALSE.
#' @param fpc
#' TRUE/FALSE.
#' Whether or not to compute a finite population correction (for variance).
#' @param weights
#' Optional.
#' If passed, it must be a vector of weights at least as long as the input y.
#'
#' @return
#' tibble with
#'  - n (sample size)
#'  - point estimate (stat: mean, total, proportion)
#'  - estimator variance (not the same as population variance)
#'  - estimator standard error
#'  - coefficient of variation
#'
#' @references
#' Lohr, Sharon L. Sampling: Design and Analysis. Chapman and Hall/CRC, 2019.
#' @export
#'
#' @examples
#' N <- 10
#' n <- 5
#' y <- c(0, 0, 0, 1, 1)
#' weights <- rep(N/n, n)
#' mk_stat(y, stat="mean", N, fpc=TRUE, weights=weights)
#' # A tibble: 1 x 5
#' #      n point   var    se    cv
#' #  <int> <dbl> <dbl> <dbl> <dbl>
#' #1     5   0.4  0.15 0.387 0.968
#'
#' N <- 10
#' n <- 5
#' y <- c(0, 0, 0, 1, 1)
#' weights <- rep(N/n, n)
#' mk_stat(y, stat="total", N, fpc=TRUE, weights=weights)
#' # A tibble: 1 x 5
#' #      n point   var    se    cv
#' #  <int> <dbl> <dbl> <dbl> <dbl>
#' #1     5     4    15  3.87 0.968
#'
#' #' N <- 10
#' n <- 5
#' y <- c(0, 0, 0, 1, 1)
#' weights <- rep(N/n, n)
#' mk_stat(y, stat="prop", N, fpc=TRUE, weights=weights)
#' # A tibble: 1 x 5
#' #      n point   var    se    cv
#' #  <int> <dbl> <dbl> <dbl> <dbl>
#' #1     5   0.4  0.03 0.173 0.433
mk_stat <- function(y, stat="mean", N=NULL, fpc=TRUE, weights=NULL){
    if(!(is.numeric(y) && length(y)>1)){
        stop(
            "y must be a numeric vector with length(y) > 1",
            call. = FALSE
        )
    }
    if(is.null(N)){
        stop(
            "N > n must be supplied (n = length(y))",
            call. = FALSE
        )
    }else{
        if(N < length(y)){
            stop(
                "N must be greater than n = length(y)",
                call. = FALSE
            )
        }
    }
    if(!(stat %in% c("mean", "total", "prop"))){
        stop(
            "stat must be one of mean, total, or prop",
            call. = FALSE
        )
    }
    n = length(y)
    if(stat == "mean"){
        tbl = list(
            n = n,
            point = .mean(y=y, weights=weights),
            var = .var_mean(y=y, N=N, fpc=fpc),
            se = .se_mean(y=y, N=N, fpc=fpc),
            cv = .cv_mean(y, N=N, fpc=fpc, weights=weights)
        )
    }else if(stat == "total"){
        tbl = list(
            n = n,
            point = .total(y=y, N=N, weights=weights),
            var = .var_total(y=y, N=N, fpc=fpc),
            se = .se_total(y=y, N=N, fpc=fpc),
            cv = .cv_total(y, N=N, fpc=fpc, weights=weights)
        )
    }else if(stat == "prop"){
        tbl = list(
            n = n,
            point = .prop(y=y, weights=weights),
            var = .var_prop(y=y, N=N, fpc=fpc),
            se = .se_prop(y=y, N=N, fpc=fpc),
            cv = .cv_prop(y, N=N, fpc=fpc, weights=weights)
        )
    }
    return(dplyr::as_tibble(tbl))
}

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

# Functions for estimating a proportion using SRS samples
.prop <- function(y, weights = NULL){
    if(!all(y %in% 0:1)){
        stop("A 0-1 variable must be supplied")
    }
    .mean(y, weights)
}

.var_p <- function(y, weights=NULL){
    p <- .prop(y, weights)
    n <- length(y)
    s2 <- n / (n-1) * p * (1 - p)
    return(s2)
}

.var_prop <- function(y, N=NULL, fpc=TRUE, weights=NULL){
    n <- length(y)
    # Only depend on N as input when computing finite population correction (fpc)
    if(fpc){
        if(is.null(N)){
            stop("N >= n must be supplied when using fpc")
        }else{
            if(N < n)
                stop("N < n provided. N >= n required.")
        }
        s2 <- (1 - n / N) * .var_p(y, weights) / n
    } else{
        s2 <- .var_p(y, weights) / n
    }
    return(s2)
}

.se_prop <- function(y, N=NULL, fpc=TRUE, weights=NULL){
    sqrt(.var_prop(y, N, fpc))
}

.cv_prop <- function(y, N=NULL, fpc=TRUE, weights=NULL){
    .se_prop(y, N, fpc) / .prop(y, weights)
}
