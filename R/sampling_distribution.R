#' sampling_distribution
#'
#' @description
#' Computes the sampling distribution of an estimator given a population,
#' sample size, and statistic.
#' The population must be a vector of numeric variables.
#' The statistic must be the sample mean, sample total, or sample proportion.
#' The function must store the entire sample as a matrix at some point, so it is
#' bound on what samples can taken by choose(N, n).
#'
#' @param y
#' A numeric vector of measurements on observations units.
#' The population, N, will be computed as the length of y.
#' @param n
#' The size of samples to draw from y.
#' @param stat
#' A point estimate ("mean", "total", or "prop") to estimate from the population.
#' Must be a string input.
#'
#' @return
#' The sampling distribution of the sample mean
#' @export
#'
#' @examples
#' y <- c(1, 1, 2, 2, 3, 3)
#' n <- 2
#' sampling_distribution(y, n, stat="mean")
#' # [1] 1.0 1.5 1.5 2.0 2.0 1.5 1.5 2.0 2.0 2.0 2.5 2.5 2.5 2.5 3.0
#'
#' y <- c(1, 1, 2, 2, 3, 3)
#' n <- 2
#' sampling_distribution(y, n, stat="total")
#' # [1]  6  9  9 12 12  9  9 12 12 12 15 15 15 15 18
#'
#' y <- c(0,0,0,0,1,1)
#' n <- 2
#' sampling_distribution(y, n, stat="prop")
#' # [1] 0.0 0.0 0.0 0.5 0.5 0.0 0.0 0.5 0.5 0.0 0.5 0.5 0.5 0.5 1.0
sampling_distribution <- function(y, n, stat="mean"){
    if(!(is.vector(y) && is.numeric(y))){
        stop("y must be a numeric vector")
    }
    if(n > length(y)){
        stop("n must be less than N = length(y)")
    }
    sampled_data <- draw_samples(y, n)$sampled_data
    if(stat == "mean"){
        sampling_dist <- apply(sampled_data, 1, .mean)
    }else if(stat == "total"){
        N <- length(y)
        sampling_dist <- apply(sampled_data, 1, .total, N=N)
    }else if(stat == "prop"){
        sampling_dist <- apply(sampled_data, 1, .prop)
    }else{
        stop(
            "stat must be one of \"mean\", \"total\", or \"prop\""
        )
    }
    return(sampling_dist)
}
