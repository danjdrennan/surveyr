test_that(".mean function handles weight input lengths", {
    # Generate a simple dataset for testing
    y <- 1:5
    w <- rep(2, length(y))
    expect_error(.mean(y, w[1]), "weights and y must be the same length")
})
test_that(".mean function computes correctly", {
    # Generate a simple dataset for testing
    y <- 1:5
    w <- rep(2, length(y))
    expect_equal(.mean(y), 3.0)
    expect_equal(.mean(y, w), 3.0)
})
test_that(".var_mean type checks work", {
    y <- 1:5
    N <- length(y)
    expect_error(
        .var_mean(y),
        "N >= n must be supplied when using fpc"
    )
    expect_error(
        .var_mean(y, N=1, fpc=TRUE),
        "N < n provided. N >= n required."
    )
})
test_that(".var_mean correctly computes for SRS", {
    y <- 1:5
    N <- length(y)
    expect_equal(.var_mean(y, N), 0.0)
    expect_equal(.var_mean(y, 10*N), 2.25)
    expect_equal(
        .var_mean(y, fpc=FALSE),
        .var_mean(y, N, fpc=FALSE)
    )
})
test_that(".se_mean type handling", {
    y <- 1:5
    N <- length(y)
    expect_error(
        .se_mean(y),
        "N >= n must be supplied when using fpc"
    )
    expect_error(
        .se_mean(y, N=1, fpc=TRUE),
        "N < n provided. N >= n required."
    )
})
test_that(".se_mean correctness checks", {
    y <- 1:5
    N <- length(y)
    expect_equal(.se_mean(y, N), 0.0)
    expect_equal(.se_mean(y, 10*N), sqrt(.var_mean(y, 10*N)))
    # must reach equality
    expect_equal(
        .se_mean(y, fpc=FALSE),
        .se_mean(y, 10*N, fpc = FALSE)
    )
})
test_that(".cv_mean error checks are correctly handled", {
    y <- 1:5
    N <- length(y)
    w <- rep(2, length(y))
    # .var_mean is in the dependency graph for this function, so all errors
    # need to be confirmed with expected behaviors
    expect_error(
        .cv_mean(y),
        "N >= n must be supplied when using fpc"
    )
    expect_error(
        .cv_mean(y, 1),
        "N < n provided. N >= n required."
    )
})
test_that(".cv_mean correctness", {
    y <- 1:5
    N <- length(y)
    w <- rep(2, length(y))
    expect_equal(.cv_mean(y, 5), 0.0)
    expect_equal(.cv_mean(y, 10*N), 0.5)
    expect_equal(
        .cv_mean(y, fpc=FALSE),
        .cv_mean(y, 10*N, fpc=FALSE)
    )
    expect_equal(.cv_mean(y, 10*N, weights=w), 0.5)
})
test_that(".total handles type errors", {
    y <- 1:3
    N <- 50
    w <- rep(50/3, 3)
    expect_error(.total(y), "N or weights must be supplied")
    expect_error(.total(y, w), "N must be an integer")
})
test_that(".total computes correctly", {
    y <- 1:3
    N <- 50
    w <- rep(50/3, 3)
    expect_equal(.total(y, N), 100)
    expect_equal(.total(y, weights=w), 100)
})
test_that(".var_total correctness", {
    y <- 1:5
    N <- 50
    expect_error(.var_total(y, 1))
    expect_equal(.var_total(y, N), N^2 * 2.25)
    expect_equal(.var_total(y, N, fpc=FALSE), N^2*2.5)
})
test_that(".se_total correctness", {
    y <- 1:5
    N <- 50
    expect_error(.se_total(y, 1))
    expect_equal(.se_total(y, N), N * 1.5)
})
test_that(".cv_error handling", {
    y <- 1:5
    N <- length(y)
    w <- rep(2, length(y))
    expect_error(.cv_total(y, 1))
})
test_that(".cv_error correctness", {
    y <- 1:5
    N <- length(y)
    w <- rep(2, length(y))
    expect_equal(.cv_total(y, 5), 0.0)
    expect_equal(.cv_total(y, 10*N), 0.5)
    expect_equal(
        .cv_total(y, fpc=FALSE),
        .cv_mean(y, 10*N, fpc=FALSE)
    )
    expect_equal(.cv_total(y, 10*N, weights=w), 0.5)
})
test_that(".prop error handling", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    expect_error(.prop(c(y, 2)), "A 0-1 variable must be supplied")
})
test_that(".prop correctness checks", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    expect_equal(.prop(y), 0.6)
    expect_equal(.prop(y, w), 0.6)
})
test_that(".var_p correctness checks", {
    #.var_p is the standard deviation of a proportion
    # This required a custom definition to make sure it was computed correctly
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    expect_equal(.var_p(y), 0.3)
    expect_equal(.var_p(y), .var_p(y, w))
})
test_that(".var_prop error handling", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    N <- 10
    expect_error(.var_prop(y), "N >= n must be supplied when using fpc")
    expect_error(.var_prop(y, 1), "N < n provided. N >= n required.")
})
test_that(".var_prop correctness checks", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    N <- 10
    expect_equal(.var_prop(y, N), 0.03)
    expect_equal(.var_prop(y, 5), 0.0)
    expect_equal(.var_prop(y, 1, fpc=FALSE), .var_prop(y, fpc=FALSE))
})
test_that(".se_prop error handling", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    N <- 10
    expect_error(.se_prop(y), "N >= n must be supplied when using fpc")
    expect_error(.se_prop(y, 1), "N < n provided. N >= n required.")
})
test_that(".se_prop correctness checks", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    N <- 10
    expect_equal(.se_prop(y, N), 0.17320508)
    expect_equal(.se_prop(y, 5), 0.0)
})
test_that(".cv_prop correctness checks", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    N <- 10
    expect_equal(.cv_prop(y, N), 0.17320508 / 0.6)
})
test_that("mk_stat passes compatibility checks", {
    expect_error(
        mk_stat(0, N=1),
        "y must be a numeric vector with"
    )
    expect_error(
        mk_stat(1:10),
        "N > n must be supplied"
    )
    expect_error(
        mk_stat(1:10, N=1),
        "N must be greater than n"
    )
    expect_error(
        mk_stat(1:10, stat="failure!", N = 12),
        "stat must be one of mean, total, or prop"
    )
})
test_that("mk_stat computes expected values for input dataset", {
    N <- 10
    n <- 5
    y <- c(0, 0, 0, 1, 1)
    weights <- rep(N/n, n)
    expect_equal(
        mk_stat(y, stat = "mean", N, fpc=TRUE, weights=weights),
        dplyr::tibble(
            n = 5,
            point = 0.4,
            var = 0.15,
            se = sqrt(var),
            cv = se / point
        )
    )
    expect_equal(
        mk_stat(y, stat = "total", N, fpc=TRUE, weights=weights),
        dplyr::tibble(
            n = 5,
            point = 4,
            var = 15,
            se = sqrt(var),
            cv = se / point
        )
    )
    expect_equal(
        mk_stat(y, stat = "prop", N, fpc=TRUE, weights=weights),
        dplyr::tibble(
            n = 5,
            point = 0.4,
            var = 0.03,
            se = sqrt(var),
            cv = se / point
        )
    )
})
