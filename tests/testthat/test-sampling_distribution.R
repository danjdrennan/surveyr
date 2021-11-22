test_that("sampling_distribution handles bad inputs", {
    expect_error(
        sampling_distribution(1:5, 10),
        "n must be less than length(y)"
    )
    expect_error(
        sampling_distribution(letters, 3),
        "y must be a numeric vector"
    )
})

test_that("sampling_distribution correctly estimates distributions", {
    y <- c(1, 3, 2, 3, 1, 2)
    n <- 3
    d <- draw_samples(y, n)
    sampling_dist <- rowMeans(d$sampled_data)
    expect_equal(sampling_distribution(y, n), sampling_dist)
})
