test_that("type checks for stratified_stat work", {
    expect_error(stratified_stat(.tbl = "This arg fails", .stat="mean"))
    expect_error(stratified_stat(.tbl = tibble::tibble(), .stat="now fails"))
})
test_that("correctness checks for stratified_stat",{
    # There are four branches to check in this code with .fpc = TRUE/FALSE
    # and .stat = "total" or not "total". If a proportion is given in the data,
    # the main estimation difference between it and a mean will settled in
    # make_summary with the variance components.

    # Start by constructing a minimal test data set
    d <- tibble::tibble(
        stratum = c(1, 1, 2, 2),
        y = c(1, 3, 1, 3),
        p = c(0, 1, 0, 1)
    )
    N <- c(10, 10)

    # Now conduct tests with .fpc=FALSE and .stat = "mean"
    make_summary(d, stratum, y, N, .stat="mean", .fpc=FALSE) %>%
        stratified_stat(.stat="mean", .fpc=FALSE) -> stratified_stat
    expected_result <- tibble::tibble(
        Ntotal = 20,
        ntotal = 4,
        point = 2,
        var = 0.5,
        se = sqrt(var),
        cv = se / point
    )
    expect_equal(
        stratified_stat,
        expected_result
    )
    # Switch arguments to .fpc=TRUE with .stat="mean"
    make_summary(d, stratum, y, N, .stat="mean", .fpc=TRUE) %>%
        stratified_stat(.stat="mean", .fpc=TRUE) -> stratified_stat
    expected_result <- tibble::tibble(
        Ntotal = 20,
        ntotal = 4,
        point = 2,
        var = 0.32,
        se = sqrt(var),
        cv = se / point
    )
    expect_equal(
        stratified_stat,
        expected_result
    )
    # Test .fpc=FALSE, .stat="total"
    make_summary(d, stratum, y, N, .stat="total", .fpc=FALSE) %>%
        stratified_stat(.stat="total", .fpc=FALSE) -> stratified_stat
    expected_result <- tibble::tibble(
        Ntotal = 20,
        ntotal = 4,
        point = 40,
        var = 20000,
        se = sqrt(var),
        cv = se / point
    )
    expect_equal(
        stratified_stat,
        expected_result
    )
    # Test .fpc=TRUE, .stat="total"
    make_summary(d, stratum, y, N, .stat="total", .fpc=TRUE) %>%
        stratified_stat(.stat="total", .fpc=TRUE) -> stratified_stat
    expected_result <- tibble::tibble(
        Ntotal = 20,
        ntotal = 4,
        point = 40,
        var = 12800,
        se = sqrt(var),
        cv = se / point
    )
    expect_equal(
        stratified_stat,
        expected_result
    )
    # Now show equivalence between .stat = "mean" and .stat = "prop" in both
    # functions
    make_summary(d, stratum, p, N, .stat="prop", .fpc=TRUE) %>%
        stratified_stat(.stat="mean", .fpc=TRUE) -> stratified_mean
    make_summary(d, stratum, p, N, .stat="prop", .fpc=TRUE) %>%
        stratified_stat(.stat="prop", .fpc=TRUE) -> stratified_prop
    expect_equal(
        stratified_mean,
        stratified_prop
    )
    make_summary(d, stratum, p, N, .stat="prop", .fpc=FALSE) %>%
        stratified_stat(.stat="mean", .fpc=FALSE) -> stratified_mean
    make_summary(d, stratum, p, N, .stat="prop", .fpc=FALSE) %>%
        stratified_stat(.stat="prop", .fpc=FALSE) -> stratified_prop
    expect_equal(
        stratified_mean,
        stratified_prop
    )
})
