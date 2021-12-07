test_that("dplyr imports with pipe operator %>%", {
    # This test will only work if dplyr is imported into the software as expected
    # and if the pipe opertor %>% works as expected.
    d <- as_tibble(cbind("a" = 1, "b" = 1:5)) %>%
        summarize("mean_a" = mean(a), "mean_b" = mean(b))
    expect_equal(d$mean_a, 1)
    expect_equal(d$mean_b, 3)
})
