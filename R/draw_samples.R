#' draw_samples
#'
#' @description
#' Draws samples from a simple population of data and returns (1) the sample
#' index and (2) the sampled values from the population. Useful for generating
#' draws from small, finite populations for pedagogical demonstrations or
#' for generating sampling distributions.
#'
#' @param y
#' Vector of population data to generate samples with.
#' The vector can be anything, numeric or otherwise, but is primarily intended
#' for working with numerical data.
#' The length of y will be used as the population size, N.
#' @param n
#' Sample size to use when drawing samples.
#'
#' @return sample_indices
#' A matrix with choose(N, n) rows and n columns.
#' Each row corresponds to a sample of the indices in the population, y.
#'
#' @return sampled_data
#' A matrix with choose(N, n) rows and n columns.
#' Each row is a sample drawn from y with sample size n.
#' @export
#'
#' @examples
#' # Example 1:
#' # ----------
#' set.seed(1)
#' y <- rnorm(10)
#' out <- draw_samples(y, 3)
#' out$sample_indices[1:3, ]
#' #     [,1] [,2] [,3]
#' #[1,]    1    2    3
#' #[2,]    1    2    4
#' #[3,]    1    2    5
#' out$sampled_data[1:3, ]
#' #           [,1]      [,2]       [,3]
#' #[1,] -0.6264538 0.1836433 -0.8356286
#' #[2,] -0.6264538 0.1836433  1.5952808
#' #[3,] -0.6264538 0.1836433  0.3295078
#'
#' # Example 2:
#' # ----------
#' y <- c(1, 3, 3)
#' out <- draw_samples(y, 2)
#' out$sample_indices
#' #     [,1] [,2]
#' #[1,]    1    2
#' #[2,]    1    3
#' #[3,]    2    3
#' out$sampled_data
#' #     [,1] [,2]
#' #[1,]    1    3
#' #[2,]    1    3
#' #[3,]    1    3
draw_samples <- function(y, n){
    if(!is.vector(y)){
        stop("y must be a vector type")
    }
    N <- length(y)
    if(n > N){
        stop("n should be a sample size smaller than N = length(y)")
    }
    sample_indices <- combn(N, n)
    sampled_data <- matrix(
        y[sample_indices],
        ncol(sample_indices),
        nrow(sample_indices),
        byrow=T
    )
    return(list(
        "sample_indices" = t(sample_indices),
        "sampled_data" = sampled_data
    ))
}
