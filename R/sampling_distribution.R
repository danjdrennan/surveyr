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
#'
#' @return
#' The sampling distribution of the sample mean
#' @export
#'
#' @examples
#' y <- c(1, 1, 2, 2, 3, 3)
#' n <- 2
#' sampling_distribution(y, n)
#' # 1.0 1.5 1.5 2.0 2.0 1.5 1.5 2.0 2.0 2.0 2.5 2.5 2.5 2.5 3.0
sampling_distribution <- function(y, n){
    if(!(is.vector(y) && is.numeric(y))){
        stop("y must be a numeric vector")
    }
    if(n > length(y)){
        stop("n must be less than N = length(y)")
    }
    sampled_data <- draw_samples(y, n)$sampled_data
    return(rowMeans(sampled_data))
}
