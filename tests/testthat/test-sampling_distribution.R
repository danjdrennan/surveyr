test_that("sampling_distribution handles bad inputs", {
    expect_error(
        sampling_distribution(letters, 3),
        "y must be a numeric vector"
    )
    expect_error(
        sampling_distribution(1:5, 10),
        "n must be less than N"
    )
    expect_error(
        sampling_distribution(1:10, 3, stat="fail"),
        "stat must be one of"
    )
    expect_error(
        # Should have 0-1 variable
        # .prop function will catch and throw error
        sampling_distribution(1:10, 3, stat="prop")
    )
})
test_that("sampling_distribution correctly estimates distributions", {
    y <- rep(1:3, 2)
    n <- 3
    d <- draw_samples(y, n)
    # First test default case with the mean
    sampling_dist <- rowMeans(d$sampled_data)
    expect_equal(sampling_distribution(y, n), sampling_dist)
    # Test for total
    sampling_dist <- apply(d$sampled_data, 1, .total, N = length(y))
    expect_equal(sampling_distribution(y, n, stat="total"), sampling_dist)
    y <- 1 * (y > 2)
    sampled_data <- 1 * (d$sampled_data > 2)
    sampling_dist <- apply(sampled_data, 1, .prop)
    expect_equal(sampling_distribution(y, n, stat="prop"), sampling_dist)
})
