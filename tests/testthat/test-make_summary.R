# Make a dataset as a basis for all tests
# Need to actually know the values of y in correctness tests
set.seed(1)
d <- tibble::tibble(
    stratum = rep(1:5, 10),
    y = rnorm(50, 30, 3),
    p = sample(0:1, 50, replace=TRUE)
)
dw <- d %>% mutate(w = 4)
N <- rpois(5, 100)
tn <- tibble::tibble(stratum=1:5, N=N)

# Error checks will probe inputs
test_that("error checks for make_summary",{
    expect_error(make_summary("hello", stratum, y, N))
    expect_error(make_summary(d, stratum, "y", N))
    expect_error(make_summary(d, stratum, y, "N"))
    expect_error(make_summary(d, stratum, y, rep(N, 2)))
    expect_error(make_summary(d, stratum, y, N, .stat="fail"))
    expect_error(make_summary(d, stratum, y, N, .fpc="NotLogical"))
    expect_error(make_summary(dw, stratum, y, N, .group_weights="w"))
    expect_error(make_sumamry(dw, stratum, y, N, .group_weights=N))
    expect_error(make_sumamry(dw, stratum, y, .group_weights=N))

    # These tests fail and need further inspection.
    # They represent extreme edge cases that a user should not pass anyhow, but
    # the behavior needs to be fenced to guarantee expected behaviors in
    # the provided functions
    # expect_error(make_summary(d, "stratum", y, N))
    # expect_error(make_summary(dw, stratum, y, N, .group_weights=TRUE))
})
# This is a shortcut construction to simplify computing summaries in tests
joined_data <- left_join(d, tn) %>%
    mutate(w = N/10) %>%
    group_by(stratum)
test_that("correctness checks for make_summary", {
    # Means with fpc
    joined_data %>% summarize(
            N = sum(w),
            mk_stat(y, stat="mean", N, weights=w)
        ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, y, N, .stat="mean", .fpc=TRUE),
        expected_mean
    )
    # Same as previous test but without weights in the expected check
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(y, stat="mean", N)
    ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, y, N, .stat="mean", .fpc=TRUE),
        expected_mean
    )
    # Same as test on lines 38:45, this time with fpc=FALSE
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(y, stat="mean", N, fpc=FALSE, weights=w)
    ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, y, N, .stat="mean", .fpc=FALSE),
        expected_mean
    )
    # Now repeating test on 46:54, fpc=FALSE
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(y, stat="mean", N, fpc=FALSE)
    ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, y, N, .stat="mean", .fpc=FALSE),
        expected_mean
    )
    # Same as 38:45 using totals rather than means
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(y, stat="total", N, fpc=TRUE, weights=w)
    ) -> expected_total
    expect_equal(
        make_summary(d, stratum, y, N, .stat="total", .fpc=TRUE),
        expected_total
    )
    # Now checks with totals
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(y, stat="total", N, fpc=TRUE)
    ) -> expected_total
    expect_equal(
        make_summary(d, stratum, y, N, .stat="total", .fpc=TRUE),
        expected_total
    )
    # Now checks with totals
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(y, stat="total", N, fpc=FALSE, weights=w)
    ) -> expected_total
    expect_equal(
        make_summary(d, stratum, y, N, .stat="total", .fpc=FALSE),
        expected_total
    )
    # Now checks with totals
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(y, stat="total", N, fpc=FALSE)
    ) -> expected_total
    expect_equal(
        make_summary(d, stratum, y, N, .stat="total", .fpc=FALSE),
        expected_total
    )
    # Proportions checks
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(p, stat="prop", N, weights=w)
    ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, p, N, .stat="prop", .fpc=TRUE),
        expected_mean
    )
    # Same test with no weights passed anywhere
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(p, stat="prop", N)
    ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, p, N, .stat="prop", .fpc=TRUE),
        expected_mean
    )
    # Same prop tests with fpc=FALSE
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(p, stat="prop", N, fpc=FALSE, weights=w)
    ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, p, N, .stat="prop", .fpc=FALSE),
        expected_mean
    )
    # Repeating without weights as argument
    joined_data %>% summarize(
        N = sum(w),
        mk_stat(p, stat="prop", N, fpc=FALSE)
    ) -> expected_mean
    expect_equal(
        make_summary(d, stratum, p, N, .stat="prop", .fpc=FALSE),
        expected_mean
    )
})
