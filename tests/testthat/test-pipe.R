test_that("pipe operator works: f(x) %>% g()", {
    f <- function(x) 3*x
    g <- function(y) y + 5
    # Expect g(f(1)) = (3*1) + 5 = 8
    # Confirms pipe operator is available in package
    expect_equal(f(1) %>% g(), 8)
})
