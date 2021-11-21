test_that(".mean functions as expected for simple random samples", {
    # Confirming expected outputs for the mean, variance, standard error, and cv
    # of a mean estimator with simple random samples.

    # Generate a simple dataset for testing
    y <- 1:5
    w <- rep(2, length(y))

    # The mean function should return equivalent values when supplying or
    # ignoring weights. These two tests check for correctly computed values.
    expect_equal(.mean(y), 3.0)
    expect_equal(.mean(y, w), 3.0)

    # If weights are supplied, they must have the same length.
    # Otherwise an error must be thrown.
    expect_error(.mean(y, w[1]), "weights and y must be the same length")
})

test_that(".var_mean functionality for simple random samples", {
    y <- 1:5
    N <- length(y)
    # Prove error handling when fpc = TRUE
    # Note N can be anything when fpc = FALSE
    expect_error(
        .var_mean(y),
        "N >= n must be supplied when using fpc"
    )
    expect_error(
        .var_mean(y, N=1, fpc=TRUE),
        "N < n provided. N >= n required."
    )
    # The first case implies a census was taken
    expect_equal(.var_mean(y, N), 0.0)
    expect_equal(.var_mean(y, 10*N), 2.25)
    # The next two tests must be equivalent
    expect_equal(
        .var_mean(y, fpc=FALSE),
        .var_mean(y, N, fpc=FALSE)
    )
})

test_that(".se_mean for simple random samples", {
    y <- 1:5
    N <- length(y)
    # .se_mean wraps .var_mean equivalent to sqrt(.var_mean)
    # all error conditions must be satisfied in this case as before
    # For the user, the difference shouldn't matter because the computations
    # are all equivalent
    expect_error(
        .se_mean(y),
        "N >= n must be supplied when using fpc"
    )
    expect_error(
        .se_mean(y, N=1, fpc=TRUE),
        "N < n provided. N >= n required."
    )
    expect_equal(.se_mean(y, N), 0.0)
    expect_equal(.se_mean(y, 10*N), sqrt(.var_mean(y, 10*N)))
    # Again, these two arguments MUST be equivalent
    expect_equal(
        .se_mean(y, fpc=FALSE),
        .se_mean(y, 10*N, fpc = FALSE)
    )
})

test_that(".cv_mean for simple random samples error handling/computations", {
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
    expect_equal(.cv_mean(y, 5), 0.0)
    expect_equal(.cv_mean(y, 10*N), 0.5)
    expect_equal(
        .cv_mean(y, fpc=FALSE),
        .cv_mean(y, 10*N, fpc=FALSE)
    )
    expect_equal(.cv_mean(y, 10*N, weights=w), 0.5)
})

test_that("estimates for total are correctly handled", {
    y <- 1:3
    N <- 50
    w <- rep(50/3, 3)
    expect_error(.total(y), "N or weights must be supplied")
    expect_error(.total(y, w), "N must be an integer")
    expect_equal(.total(y, N), 100)
    expect_equal(.total(y, weights=w), 100)
})

test_that("variance for a total in SRS design passes", {
    y <- 1:5
    N <- 50
    expect_error(.var_total(y, 1))
    expect_equal(.var_total(y, N), N^2 * 2.25)
    expect_equal(.var_total(y, N, fpc=FALSE), N^2*2.5)
})

test_that("se for total in SRS passes", {
    y <- 1:5
    N <- 50
    expect_error(.se_total(y, 1))
    expect_equal(.se_total(y, N), N * 1.5)
})

test_that("cv for a total computes expectedly", {
    y <- 1:5
    N <- length(y)
    w <- rep(2, length(y))
    expect_error(.cv_total(y, 1))
    expect_equal(.cv_total(y, 5), 0.0)
    expect_equal(.cv_total(y, 10*N), 0.5)
    expect_equal(
        .cv_total(y, fpc=FALSE),
        .cv_mean(y, 10*N, fpc=FALSE)
    )
    expect_equal(.cv_total(y, 10*N, weights=w), 0.5)
})

test_that("proportion function for SRS functions correctly", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    expect_error(.prop(c(y, 2)), "A 0-1 variable must be supplied")
    expect_equal(.prop(y), 0.6)
    expect_equal(.prop(y, w), 0.6)

    expect_equal(.var_p(y), 0.3)
    expect_equal(.var_p(y), .var_p(y, w))
})

test_that("proportion variance functions for SRS function as expected", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    N <- 10
    expect_error(.var_prop(y), "N >= n must be supplied when using fpc")
    expect_error(.var_prop(y, 1), "N < n provided. N >= n required.")
    expect_equal(.var_prop(y, N), 0.03)
    expect_equal(.var_prop(y, 5), 0.0)
    expect_equal(.var_prop(y, 1, fpc=FALSE), .var_prop(y, fpc=FALSE))
})

test_that("SE(p) is correctly estimated", {
    y <- c(0, 0, 1, 1, 1)
    w <- rep(3, length(y))
    N <- 10
    expect_error(.se_prop(y), "N >= n must be supplied when using fpc")
    expect_error(.se_prop(y, 1), "N < n provided. N >= n required.")
    expect_equal(.se_prop(y, N), 0.17320508)
    expect_equal(.se_prop(y, 5), 0.0)
})
