## Will demonstrate the CLT in three cases:
## (1) A bimodal distribution
## (2) A Gamma(6, 20) distribution
## (3) A uniform distribution
## The goal is to demonstrate to a user that the sample mean of a few different
## classes of distributions all converge under iid assumptions.

#' Central Limit Theorem Demo
#'
#' @description
#' Provides a flexible demonstration of the central limit theorem. Particularly,
#' this function uses the theoretical parameters from one of three specific
#' distributions to show the distribution of the sample mean approaches a normal
#' distribution. The user can choose between three distributions:
#'
#' * binormal A binormal distribution \eqn{X|Z \sim N(Z, s)}
#' * uniform A uniform distribution such that \eqn{X \sim U[a, b]}
#' * gamma A gamma distribution, \eqn{X \sim \Gamma(a, b)}
#'
#' The key to the demonstration is that the parameters from each of these
#' distributions is known.
#'
#' Theorem (Central Limit Theorem). Let \eqn{X_1, \ldots} be a sequence of
#' independent and identically distributed random variables with mean
#' \eqn{E X_i = \mu} and \eqn{Var X_i = \sigma^2} for each \eqn{i}. Define
#' \eqn{\bar{X}_n = n^{-1}(X_1 + \cdots X_n)}. Then
#' \deqn{\frac{\lim_{n\to\infty} \sqrt{n}\bar{X}_n - \mu}{\sigma} \to N(0,1).}
#'
#' The central limit theorem essentially guarantees that, for a large enough
#' sample size, the distribution of the mean from a probability-based sample
#' converges to a standard normal distribution. This is the basis for using
#' normal and t-distributed confidence intervals.
#'
#' @param n
#' The sample size to draw from a specified distribution. This defaults to n=30,
#' but can be experimented with to see what the distribution looks like when
#' under larger and smaller sample sizes. This method requires n >= 2.
#' @param N
#' The total number of replications to make in the simulation. This number is
#' saying how many data points to use when constructing the histogram, so it should
#' be fairly large. This method must be N >= 100.
#' @param distribution
#' One of "binormal", "uniform", or "gamma".
#' @param a
#' In each distribution, this has a different function.
#'
#' * binormal The mean of one of the normal distributions
#' * uniform  The left endpoint of a uniform distribution
#' * gamma    The shape parameter in a gamma distribution
#'
#' @param b
#' As with a, b specifies a specific parameter in a distribution. Note in the
#' binormal case, a = b will produce a unimodal normal distribution.
#'
#' * binormal The mean of one of the normal distributions
#' * uniform  The right endpoint of a uniform distribution
#' * gamma    The rate parameter of a uniform distribution
#'
#' @param s
#' The standard deviation for a normal distribution. Can be ignored or specified
#' as NULL when using other distributions, although this isn't strictly necessary.
#'
#' @references
#' Billingsley, Patrick. Probability and measure. John Wiley & Sons, 2008.
#'
#' @return
#' A list with
#'
#' * data Samples drawn from the specified distribution
#' * plot A histogram of the results
#' * popmean The population mean for the specified distribution
#' * popvariance The population variance for the specified distribution
#'
#' @export
#'
#' @examples
#' # Can be run without passing any arguments
#' clt <- clt_demo()
#' # Use clt$plot to obtain the graph
clt_demo <- function(
    n=20,
    N=10000,
    distribution="binormal",
    a=30,
    b=50,
    s=6
){
    # Type checks on n and N
    stopifnot(
        is.numeric(n),
        length(n) == 1,
        is.numeric(N),
        length(N) == 1,
        n <= N,
        n >= 2,
        N >= 100
    )
    # Force integers for n and N
    n <- round(n)
    N <- round(N)

    # Ensure single numeric values for a, b, s
    stopifnot(
        is.numeric(a),
        is.numeric(b),
        is.numeric(s) | is.null(s),
        length(a) == 1,
        length(b) == 1,
        length(s) == 1 | length(s) == 0
    )

    # Confirm distribution before looping
    match.arg(distribution, c("binormal", "uniform", "gamma"))

    # Get theoretical values for mean and variance
    # And construct replications of the sample mean from the theoretical
    # distribution
    if(distribution == "binormal"){
        # Make sure s is validly specified
        if(s < 0){
            stop("s must be nonnegative in binormal distribution")
        }else if(is.null(s)){
            stop("s must be specified to use a binormal distribution")
        }
        # If X_i | Z_i ~ N(Z_i, S^2), P[Z=a] = P[Z = b], i = 1, ..., N:
        D <- .rbinormal(N, a, b, s)
        # E X = (b+a)/2,
        # Var X = S
        M <- (b + a)/2
        V <- s^2
        # Replicated mean estimates
        Y <- replicate(N, mean(.rbinormal(n, a, b, s)))
    }else if(distribution == "uniform"){
        # X_i ~ U[a, b], i = 1, ..., N:
        D <- runif(N, min=a, max=b)
        # E X = (b+a)/2,
        # Var X = (b^2 + ab + a^2)/3 + (b-a)(b+a)^2/4
        M <- (b + a)/2
        V <- (b^2 + a*b + a^2)/3 - (b-a)*(b + a)^2/4
        # Replicated mean estimates
        Y <- replicate(N, mean(runif(n, min=a, max=b)))
    }else if(distribution == "gamma"){
        # X ~ Gamma(shape = a, scale = b), i = 1, ..., N:
        D <- rgamma(N, shape=a, rate=b)
        # E X = ab,
        # VarX = ab^2
        M <- a * b
        V <- a * b^2
        # Replicated mean estimates
        Y <- replicate(N, mean(rgamma(n, shape=a, rate=b)))
    }
    # Compute standardized variables using theoretical mean and variance, which
    # will be returned to user in a list
    X <- sqrt(N) * (Y - M) / V

    # Derive a suggested number of bins using the larger of Doane's formula
    # or Sturge's formula
    nb <- max(
        .vbins(X),
        ceiling(log2(N)) + 1
    )

    # Construct histogram to output
    X %>% tibble::as_tibble() %>%
        ggplot2::ggplot(ggplot2::aes(x=value)) +
        ggplot2::geom_histogram(color="gray", bins = nb) +
        ggplot2::labs(
            title = "Central Limit Theorem Demonstration",
            subtitle = paste(N, "replications from", distribution, "distribution"),
            x = "Standardized Mean",
            y = "Frequency",

        ) -> G
    return(list("data"=D, "plot"=G, "popmean"=M, "popvariance"=V))
}

# Define a function to sample from a binormal distribution X|Z ~ N(Z, S^2)
.rbinormal <- function(N, m1=30, m2=50, s=6){
    # N is the size of the population to sample
    Z <- sample(c(m1, m2), N, replace=T)
    X <- rnorm(N, Z, s)
    return(X)
}

# Adapt .bins to work with a vector of data
.vbins <- function(y){
    # get values needed to estimate bin count
    n <- length(y)
    m <- mean(y)
    s <- sd(y)
    k <- .cubed_diff(y)
    sk <- sqrt(6*(n-2) / ((n + 1)*(n + 3)))
    1 + log2(n) +
        log2(1 + abs(k) / sk) %>%
        round() -> n_bins
    return(n_bins)
}
