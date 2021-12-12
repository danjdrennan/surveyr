#' Law of Large Numbers Demo
#'
#' @description
#' Provides a demonstration of the law of large numbers using draws from a
#' gamma distribution with shape 20 and scale 1/20, i.e.
#' \eqn{Y_i \sim \Gamma(k=6, \theta=20), i=1,2,\ldots}.
#' The mean and variance, then, are \eqn{E Y_i = k\theta = 120} and
#' \eqn{\mathrm{Var} Y_i = k\theta^2 = 2400} for each i.
#' Draws from the distribution used here can be made by `rgamma(n, 6, 1/20)`;
#' see `?rgamma` for more details about the function.
#' A version of the Law of Large Numbers is given below.
#'
#' Theorem (Law of Large Numbers): Let \eqn{Y_1, \ldots} be independent and
#' identically distributed random variables with expectation \eqn{E Y_i =\mu}
#' for all i. Define \eqn{S_n = Y_1 + \cdots + Y_n}.
#' Then \eqn{\bar{Y}_n = n^{-1}S_n \to \mu} in probability.
#' A similar statement holds for convergence to the variance.
#'
#' The Law of Large Numbers is useful because it gives conditions under which
#' an estimator converges to its population value.
#'
#' @param n
#' A positive integer greater than 100.
#'
#' @return
#' A list with
#' \item{data}{The constructed dataset used for plotting}
#' \item{plot}{A ggplot plot object showing convergence of the mean and variance}
#'
#' @references
#' Billingsley, Patrick. Probability and measure. John Wiley & Sons, 2008.
#'
#' @export
#'
#' @examples
#' lln_demo()
#'
lln_demo <- function(n=5000){
    # Some simple constraints on n (number of sample draws)
    stopifnot(
        is.numeric(n),
        length(n) == 1,
        n > 100
    )

    # In case the n supplied is not an integer, cast it as such
    n <- round(n)

    # Forcing the demo with a gamma(6, 1/20) distribution for the demo because
    # I want a distribution with some curvature and skewness, but also to obtain
    # the theoretical parameter values being estimated using the LLN
    gamma_mean <- 6 * 20
    gamma_var <- 6 * 20^2
    y <- rgamma(n+10, shape = 6, scale = 20)

    # Now construct a long dataset of cumulative mean and variance estimates
    tibble::tibble(
        index = rep(1:n, 2),
        stats = c(rep("mean", n), rep("variance", n)),
        estimates = c(.cmean(y)[-c(1:10)], .cvar(y)[-c(1:10)])
    ) %>% tidyr::drop_na() -> d

    # Faceted plotting arguments
    d %>%
        ggplot2::ggplot(ggplot2::aes(x = index, y = estimates)) +
        ggplot2::facet_wrap(~stats,scales = "free") +
        ggplot2::geom_point(size=1.2) +
        ggplot2::labs(
            title = "Law of Large Numbers",
            subtitle = "Demo sampling from A gamma distribution with y = rgamma(n, shape=6, rate=1/20)",
            x = "Index (Arbitrary)",
            y = "Parameter Estimates"
        ) +
        ggplot2::geom_abline(
            slope = c(0, 0),
            intercept=c(gamma_mean, gamma_var),
            color = "red",
            size = 1
        ) -> G
    return(list("data" = d, "plot" = G))
}

.cmean <- function(x){
    cumsum(x) / 1:length(x)
}

.cvar <- function(x){
    .m <- .cmean(x)
    .d2 <- (x - .m)^2
    cv <- cumsum(.d2) / (1:length(x) - 1)
}
