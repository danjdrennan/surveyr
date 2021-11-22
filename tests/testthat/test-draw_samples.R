test_that("draw samples checks", {
    # Generate simple data for testing
    y <- c(1, 3, 3)
    n <- 2
    sample_indices <- matrix(c(1, 1, 2, 2, 3, 3), 3, 2)
    sampled_data <- matrix(c(1, 1, 3, 3, 3, 3), 3, 2)
    draw <- draw_samples(y, n)
    # Prove first dataset output
    expect_equal(draw$sample_indices, sample_indices)
    expect_equal(draw$sampled_data, sampled_data)
    # Show error for non-vector input
    expect_error(
        draw_samples(NULL, 0),
        "y must be a vector type"
    )
    # Show error when n > N
    expect_error(
        draw_samples(y, 100),
        "n should be a sample size smaller than N"
    )
})
