# We won't write better implementations of the L^2 functions than the ones in
# stats.
# Those packages already do excellent error handling.
# Thus, we will import them as dependencies.
# This decision could be changed in a future version of the package with minimal
# impact on the user.
test_that("var, sd, cov, cor are in NAMESPACE", {
    x <- -1:1
    expect_equal(var(x), 1.0)
    expect_equal(sd(x), 1.0)

    y <- -1:1
    expect_equal(cov(x, y), 1.0)
    expect_equal(cor(x, y), 1.0)
})
